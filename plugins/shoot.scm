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

(define-module (cunning-bot plugins shoot)
  #:use-module (cunning-bot bot)
  #:export (shoot))

(define random-state (random-state-from-platform))

(define firearms
  (call-with-input-file
      (%search-load-path "cunning-bot/plugins/firearms.sexp")
    read))

(define (random-firearm)
  (vector-ref firearms (random (vector-length firearms) random-state)))

(define (shoot bot sender args)
  "shoot NICK : shoots NICK with a randomly chosen novelty firearm"
  (define firearm (random-firearm))
  (make-action (format #f "loads its ~a and ~a ~a"
                       (car firearm)
                       (cdr firearm)
                       args)))
