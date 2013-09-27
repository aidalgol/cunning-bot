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

(define-module (cunning-bot plugins help)
  #:use-module (ice-9 documentation)
  #:use-module (cunning-bot bot)
  #:export (help commands dont-help))

(define admin-command? (@@ (cunning-bot plugins admin) admin-command?))

(define bot-command (@@ (cunning-bot bot) bot-command))

(define (help bot sender args)
  "help COMMAND : returns the documentation for COMMAND"
  (define command-name
    (string->symbol (string-trim args)))
  (cond ((bot-command bot command-name) =>
         (lambda (var)
           (or (and=> (object-documentation var)
                      (lambda (doc)
                        (if (admin-command? var)
                            (string-append "(admin) " doc)
                            doc)))
               (format #f "no help for command: ~a" command-name))))
        (else (format #f "no such command: ~a" command-name))))

(define get-commands (@@ (cunning-bot bot) get-commands))
(define admin-command? (@@ (cunning-bot plugins admin) admin-command?))
(define is-master? (@@ (cunning-bot plugins admin) is-master?))

(define* (command-names bot #:key (admin? #f))
  (define command-module (get-commands bot))
  (define imports (module-uses command-module))
  (define h (make-hash-table))
  (define add!
    (if admin?
        (lambda (key value)
          (hash-set! h key #t))
        (lambda (key value)
          (unless (admin-command? (variable-ref value))
            (hash-set! h key #t)))))
  (for-each (lambda (module)
              (hash-for-each add! (module-obarray module)))
            (cons command-module imports))
  (hash-map->list (lambda (key value) key) h))

(define (commands bot sender args)
  "COMMANDS : returns a list of all the bot commands"
  (define commands
    (command-names bot #:admin? (is-master? sender)))
  (string-append sender
                 ": "
                 (string-join (map symbol->string commands) ", ")))

(define (dont-help . _) (make-action "blows a raspberry"))
