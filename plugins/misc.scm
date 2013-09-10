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

(define-module (cunning-bot plugins misc)
  #:use-module (cunning-bot bot)
  #:export (source ping botsnack))

(define (source bot sender args)
  "git clone git://github.com/aidalgol/cunning-bot.git")

(define (ping bot sender args)
  "pong")

(define (botsnack bot sender args)
  (random-botsnack))

(define (random-botsnack)
  (vector-ref botsnack-responses (random (vector-length botsnack-responses))))

(define botsnack-responses
  (vector
   ;; fsbot "standard" responses
   "yay!"
   ":)"
   (make-action "dances happily")
   "thank you!"
   (make-action "beams")
   "my favourite snack!"

   ;; rudybot proprietary extensions

   ;;; ungrateful
   "yech, generic brand"
   "barely even a mouthful"
   "do I look like I eat vegan botsnacks?"

   ;;; grateful
   "you, sir, are a gent of the highest calibre"
   "thank you and one day I will return the favour"
   "OMG that's just what I needed LOL"

   ;;; other
   "yow!"
   "this is going straight to my thighs"
   "come on man, one more. I need my fix!"
   ))
