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

(define-module (cunning-bot plugins basically)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (cunning-bot bot)
  #:export (basically))

(define uri (string->uri "http://itsthisforthat.com/api.php?text"))

(define* (basically bot sender args)
  "basically : explains your startup"
  (call-with-values
      (lambda () (http-get uri))
    (lambda (response body)
      body)))
