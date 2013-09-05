#!/usr/bin/env guile
!#
;; Cunning Bot, an IRC bot written in Guile Scheme.
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

(use-modules (cunning-bot bot)
             (system repl server)
             (cunning-bot commands))

(define socket-file-name "cbot-repl-socket")
(define bot (make-bot "Cunning_Bot" "Cunning_Bot" "Cunning Bot" "irc.example.net" 6667))
(for-each (lambda (command)
            (let ((name (car command))
                  (proc (cdr command)))
             (register-command! bot name proc)))
          `((flay . ,flay)
            (say-hello . ,say-hello)))

(spawn-server (make-unix-domain-server-socket #:path socket-file-name))

;; Does not work because of bug#13018.  Fixed in trunk.  See
;; https://lists.gnu.org/archive/html/bug-guile/2013-08/msg00003.html
;;
;; (sigaction SIGINT
;;   (lambda ()
;;     (quit-irc bot)
;;     (delete-file socket-file-name)))

(add-quit-hook! bot (lambda () (delete-file socket-file-name)))
(start-bot bot '("#cunning-bot"))
