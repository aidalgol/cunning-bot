;; This file is part of Cunning Bot, an IRC bot written in Guile Scheme.
;; Copyright (C) 2013 Ian Price

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

(define-module (cunning-bot plugins admin)
  #:use-module (cunning-bot bot)
  #:use-module (ice-9 match)
  #:use-module (ice-9 documentation)
  #:export (debug auth revoke (my-quit . quit) join privmsg))

;; Should be bot local
(define *master* #f)

(define (is-master? s)
  (and *master* (string-ci=? s *master*)))

(define (fail-if-not-master s)
  (or (is-master? s)
      ;;(throw 'auth-fail "You need to be my master to do this")
      (error "You need to be my master to do this" s)))

(define admin-command? (make-object-property))

(define (decorate-admin proc)
  (define new-proc
    (lambda (bot sender args)
      (fail-if-not-master sender)
      (proc bot sender args)))
  (set-object-property! new-proc 'documentation (object-documentation proc))
  new-proc)

(define-syntax-rule (define-admin (function . args) . body)
  (begin
    (define function (decorate-admin (lambda args . body)))
    (set! (admin-command? function) #t)))

(define *auth-state* #f)

(define (is-auth? s)
  (and *auth-state* (string-ci=? s *auth-state*)))

(define hex-chars "0123456789abcdef")

(define (new-auth-state)
  (define s
    (string-tabulate (lambda (_)
                       (string-ref hex-chars (random 16)))
                     16))
  (set! *auth-state* s)
  s)

(define (auth bot sender args)
  "auth [auth-string] : allows you to become the master, if you know the code"
  (match (string-tokenize args)
    (()
     (format #t "copy and paste the following line in your irc client to authenticate:\n")
     (format #t "/msg ~a auth ~a~%" (get-nick bot) (new-auth-state))
     "New authentication string created. Check stdout.")
    ((str)
     (cond ((is-auth? str)
            (set! *master* sender)
            (set! *auth-state* #f)
            "welcome, my master")
           (else
            (format #t "new auth string:~a~%" (new-auth-state))
            "Authentication failed. New auth string created.")))
    (_ "command expects only one arg")))

(define (revoke bot sender args)
  "revoke : revokes privileges for current master"
  (fail-if-not-master sender)
  (set! *master* #f)
  "done.")

(define (set-debugging! val)
  (set! (@@ (cunning-bot bot) debugging) val))

(define (toggle-debugging!)
  (set! (@@ (cunning-bot bot) debugging)
        (not (@@ (cunning-bot bot) debugging))))

(define-admin (debug bot sender args)
  "debug [on|off] : turns debugging on or off. Toggles if no argument given."
  (match (string-downcase (string-trim-both args))
    ("on" (set-debugging! #t) "done")
    ("off" (set-debugging! #f) "done")
    ("" (toggle-debugging!) "toggled")
    (other (string-append "option not supported: " other))))

(define-admin (my-quit bot sender args)
  "quit : quits the server, and shuts down bot"
  (quit-irc bot))

(define-admin (join bot sender args)
  "join [CHANNEL] : joins one or more new channels"
  (join-channels bot (string-tokenize args))
  "done.")

(define (string->object s)
  (call-with-input-string s read))

(define *guile-module* (resolve-module '(guile)))
(define-admin (my-eval bot sender args)
  "eval SEXP : evals one sexp in (guile)"
  (call-with-values
      (lambda ()
        (eval (string->object args) *guile-module*))
    (case-lambda
      (() "No values returned.")
      ((a) (object->string a))
      ((a . b) (format #f "multiple values, returning first: ~s" a)))))

