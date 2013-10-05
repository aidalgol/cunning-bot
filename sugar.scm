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

(define-module (cunning-bot sugar)
  #:use-module (cunning-bot bot)
  #:use-module (cunning-bot plugins)
  #:export (define-bot bot))

(define-syntax-rule (define-bot name . rest)
  (define name (bot . rest)))

;; First draft. It might be a better idea to use macros, because then
;; we can enforce mandatory arguments (like #:nick) in a saner way.
(define* (bot #:key (nick "examplebot")
                    (realname nick)
                    (username nick)
                    (server "irc.example.net")
                    (port 6667)
                    (plugins '())
                    (commands '()))
  (define bot
    (make-bot nick server port #:username username #:realname realname))
  (use-plugins! bot plugins)
  (register-commands! bot commands)
  bot)

(define (register-commands! bot alist)
  (for-each (lambda (command)
              (let ((name (car command))
                    (proc (cdr command)))
                (register-command! bot name proc)))
            alist))
