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

(define-module (cunning-bot plugins ddg)
  #:use-module (ddg)
  #:export ((ddg-define . define)))

(define (one-word? s)
  (not (string-index s char-whitespace?)))

(define (ddg-define bot sender args)
  "define WORD : returns the dictionary definition of WORD"
  (define args* (string-trim-both args))
  (if (not (one-word? args*))
      (string-append sender ": One word at a time, please.")
      (let* ((data (zero-click (string-append "!define " args*)))
             (definition (hash-ref data "Definition"))
             (url-string (hash-ref data "DefinitionURL")))
        (cond ((string-null? definition)
               (format #f "A definition for ~a is not available" args*))
              ((string-null? url-string) definition)
              (else
               (format #f "~a\nDefinition courtesy of ~a"
                       definition url-string))))))
