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

(define-module (cunning-bot plugins repl)
  #:use-module (system repl server)
  #:export (setup! teardown!))

(define default-socket-name
  "/tmp/cbot-repl-socket") ;; autogen it?

(define socket-file #f)

(define* (setup! bot #:optional (socket-name default-socket-name))
  (set! socket-file socket-name)
  (spawn-server (make-unix-domain-server-socket #:path socket-name)))

(define (teardown! bot)
  ;; TODO: we need a way to do cleanup on fail
  (delete-file socket-file)
  (set! socket-file! #f))
