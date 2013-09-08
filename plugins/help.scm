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

(define-module (cunning-bot plugins help)
  #:use-module (ice-9 documentation)
  #:use-module (cunning-bot bot)
  #:export (help))

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
