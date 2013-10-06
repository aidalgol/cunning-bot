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

(define-module (cunning-bot repl)
  #:use-module (system repl server)
  #:export (start-repl shutdown-repl))

(define default-socket-name
  "/tmp/cbot-repl-socket") ;; autogen it?

(define socket-file-name #f)

(define* (start-repl #:optional (socket-name default-socket-name))
  (set! socket-file-name socket-name)
  (catch 'system-error
    (lambda ()
      (spawn-server (make-unix-domain-server-socket #:path socket-name)))
    (lambda (key subr message args rest)
      (display "Error during REPL startup:") (newline)
      (display-error current-error-port subr message args rest))))

(define (shutdown-repl bot)
  (catch system-error
    (lambda ()
      (delete-file socket-file)
      (set! socket-file! #f))
    (lambda (key subr message args rest)
      (display "Error during REPL shutdown:") (newline)
      (display-error current-error-port subr message args rest))))
