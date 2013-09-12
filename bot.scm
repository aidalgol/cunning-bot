;; This file is part of Cunning Bot, an IRC bot written in Guile Scheme.
;; Copyright (C) 2011  Aidan Gauland

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cunning-bot bot)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (spells network)
  #:export (make-bot
            send-privmsg
            make-action
            register-command!
            join-channels
            quit-irc
            add-quit-hook!
            add-privmsg-hook!
            remove-privmsg-hook!
            start-bot))

(define line-end "\r\n")
(define version "Cunning Bot v0.1")
(define debugging #f) ;; Whether to print debugging messages.

(define bot-type (make-record-type "bot" '(nick username realname server port conn commands privmsg-hook quit-hook)))

(define (valid-nick/username/realname? string)
  "Returns whether STRING is a valid nick, username, or realname."
  (and (string? string)
       (not (string-null? string))))

(define* (make-bot nick server port #:key (username nick) (realname nick))
  (define privmsg-hook (make-hook 5))
  (let ((bot ((record-constructor bot-type) #f #f #f server port #f
              (resolve-module (list (gensym "bot-commands:")))
              privmsg-hook (make-hook 1))))
    (set-nick bot nick)
    (set-username bot username)
    (set-realname bot realname)
    (add-hook! privmsg-hook version-respond)
    (add-hook! privmsg-hook handle-commands)
    bot))

(define get-server (record-accessor bot-type 'server))
(define get-port (record-accessor bot-type 'port))

(define get-nick (record-accessor bot-type 'nick))
(define (set-nick bot nick)
  (if (not (valid-nick/username/realname? nick))
      (error "Invalid nick.")
      (begin
        ((record-modifier bot-type 'nick) bot nick)
        ;; Send NICK change message to the server if we're connected.
        (let ((out (get-bot-out-port bot)))
          (when (and (port? out)
                     (not (port-closed? out)))
            (irc-send bot (format #f "NICK ~a" nick)))))))

(define get-username (record-accessor bot-type 'username))
(define (set-username bot username)
  (if (not (valid-nick/username/realname? username))
      (error "Invalid username.")
      ((record-modifier bot-type 'username) bot username)))

(define get-realname (record-accessor bot-type 'realname))
(define (set-realname bot realname)
  (if (not (valid-nick/username/realname? realname))
      (error "Invalid realname.")
      ((record-modifier bot-type 'realname) bot realname)))

(define get-commands (record-accessor bot-type 'commands))
(define (register-command! bot name function)
  (module-define! (get-commands bot) name function))

(define (bot-command bot command-name)
  (define commands (get-commands bot))
  (cond ((module-variable commands command-name) => variable-ref)
        (else #f)))

(define get-conn (record-accessor bot-type 'conn))
(define set-conn (record-modifier bot-type 'conn))

(define (get-bot-out-port bot)
  (let ((conn (get-conn bot)))
    (when (connection? conn)
      (connection-output-port conn))))

(define (get-bot-in-port bot)
  (let ((conn (get-conn bot)))
    (when (connection? conn)
      (connection-input-port conn))))

(define (disconnect-bot bot)
  (let ((conn (get-conn bot)))
    (when (connection? conn)
      (close-connection conn))))

(define (connect-bot bot)
  (disconnect-bot bot)
  (let ((server (get-server bot))
        (port (get-port bot)))
    (format #t "Establishing TCP connection to ~a on port ~d..."
            server port)
    (set-conn bot (open-tcp-connection server port)))
  (format #t "done.~%"))

(define-syntax-rule (debug bot str exp ...)
  (when debugging
    (format #t (string-append "~a@~a: " str) (get-nick bot) (get-server bot) exp ...)))

;; `privmsg-hook' is run with the arguments (bot sender target message ctcp).
(define bot-privmsg-hook (record-accessor bot-type 'privmsg-hook))
(define (add-privmsg-hook! bot proc)
  (add-hook! (bot-privmsg-hook bot) proc))
(define (remove-privmsg-hook! bot proc)
  (remove-hook! (bot-privmsg-hook bot) proc))

(define bot-quit-hook (record-accessor bot-type 'quit-hook))
(define (add-quit-hook! bot thunk)
  (add-hook! (bot-quit-hook bot) thunk))

(define (irc-send bot string)
  "Send STRING to the IRC server associated with BOT."
  (debug bot "Sending line: ~s~%" string)
  (format (get-bot-out-port bot)
          "~a~a" string line-end))

(define (read-line-irc bot)
  "Read a line from the IRC connection associated with BOT, dropping
the trailing CRLF."
  (let ((line (read-line (get-bot-in-port bot))))
    (unless (eof-object? line)
      (set! line (string-drop-right line 1))
      (debug bot "Read line ~s~%" line))
    line))

(define (pong line)
  "Return a PING response to the ping represented by LINE.
LINE should be an IRC PING command from the server."
  (format #f "PONG ~a" (substring line 6)))

(define (send-privmsg bot message target)
  "Send a PRIVMSG MESSAGE to TARGET."
  (if (string-any (char-set #\nul #\return #\newline) message)
      (error "Invalid message characters.")
   (irc-send bot (format #f "PRIVMSG ~a :~a" target message))))

(define (make-action message)
  "Wrap CTCP ACTION markup around MESSAGE."
  (format #f "\x01ACTION ~a\x01" message))

(define (join-channels bot channels)
  "Send a JOIN request for CHANNELS.

This does not (yet) handle JOIN responses, so errors are silently
ignored."
  (irc-send bot (format #f "JOIN ~a"
                        (string-join channels ","))))

(define (quit-irc bot)
  "Send a QUIT message to the server (to cleanly disconnect)."
  (format #t "Quitting...~%")
  (irc-send bot "QUIT"))

(define (process-line bot line)
  "Process a line from the IRC server."
  (cond
   ;; PONG PINGs.
   ((string-match "^PING" line)
    (irc-send bot (pong line)))
   ;; PRIVMSGs
   ((string-match "^:(.*)!.*@.* PRIVMSG (.*) :(.*)" line) =>
    (lambda (match)
      (handle-privmsg bot
                      (match:substring match 1)
                      (match:substring match 2)
                      (match:substring match 3))))))

(define (handle-privmsg bot sender target message)
  "Parse and respond to PRIVMSGs."
  (let* ((match (string-match "\x01(.*)\x01" message))
         (ctcp (if match
                   #t #f)))
    (when ctcp
      (set! message (match:substring match 1)))
    (debug bot "~:[Message~;CTCP message~] received from ~s sent to ~s: ~s~%"
           ctcp sender target message)
    (debug bot "Running PRIVMSG hook.~%")
    (run-hook (bot-privmsg-hook bot) bot sender target message ctcp)))

;; Command procedure names are the command name prepended with cmd-
(define (handle-commands bot sender target message ctcp)
  "Parse and execute command invocations.

If MESSAGE is a command invocation, then attempt to execute it,
catching and reporting any errors."
   (let* ((nick (get-nick bot))
          (match (string-match (format #f "(~a: )?(\\S*)\\s*(.*)" nick)
                               message))
         (line-prefix (match:substring match 1))
         (direct (string=? nick target))
         (recipient (if direct sender target))
         (command
          (string->symbol (match:substring match 2)))
         (args (match:substring match 3)))
    ;; Only respond if the message was sent directly to me or it is
    ;; prefixed with my nick (i.e. "nick: cmd ...").
    (when (and match
               (or direct line-prefix)
               (not ctcp))
      (debug bot "Received command invocation; looking up ~s~%" command)
      ;; Try to execute the command procudure.  If there is no such
      ;; procedure, then reply with an error message saying so.
      (catch #t
        (lambda ()
          (let ((proc (bot-command bot command)))
            (if proc
                (let ((result (proc bot sender args)))
                  (debug bot "Command ran successfully.~%")
                  (when (string? result)
                    (send-privmsg bot result recipient)))
                (error "No such command:" command))))
        (lambda (key subr message args rest)
          (debug bot "The command failed. :(~%")
          (send-privmsg bot (apply format (append (list #f message) args))
                        ;; If the command was sent directly to me, then
                        ;; reply directly to the sender, otherwise,
                        ;; assume it was sent to a channel and reply to
                        ;; the channel.
                        recipient))))))

(define (version-respond bot sender target message ctcp)
  "Respond to CTCP VERSION requests."
  (when (and ctcp
             (string=? "VERSION" message))
    (debug bot "Responding to VERSION request sent by ~s~%" sender)
    (irc-send bot (format #f "NOTICE ~a :~a" sender version))))

(define (start-bot bot channels)
  ;; Establish TCP connection.
  (connect-bot bot)

  ;; Setup the IRC connection.
  (display "Setting up IRC connection...") (debug bot "~%")
  (irc-send bot (format #f "NICK ~a" (get-nick bot)))
  (irc-send bot (format #f "USER ~a 0 * :~a"
                        (get-username bot) (get-realname bot)))
  ;; We should now have received responses 001-004 (right after the
  ;; NOTICEs).  If not, then quit.
  (let lp ((line (read-line-irc bot))
           (last-msg-num #f))
    (if (eof-object? line)
        (begin
          (format #t "Error: Connection closed.~%")
          (quit bot)))
    (if (not last-msg-num)
        ;; Start counting responses when we reach the first one.
        (if (string-match "^:.* 001.*" line)
            (set! last-msg-num 0)
            (lp (read-line-irc bot)
                last-msg-num))
        ;; Verify that we received all expected responses.
        (when (and (string-match (format #f "^:.* ~3'0d.*" (1+ last-msg-num)) line)
                   (< last-msg-num 4))
          (lp (read-line-irc bot)
              (1+ last-msg-num)))))
  (display "done.") (newline)
  ;; We are now connected to the IRC server.

  ;; Join channels.
  (display "Joining channels...")
  (join-channels bot channels)
  (format #t "done.~%")

  ;; Enter the message-polling loop.
  (do ((line (read-line-irc bot) (read-line-irc bot)))
      ((eof-object? line)
       (run-hook (bot-quit-hook bot) bot))
    (process-line bot line)))
