#!/usr/bin/env guile
!#
;; This file is part of Cunning Bot, an IRC bot written in Guile Scheme.
;; Copyright (C) 2011,2013 Aidan Gauland
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

(use-modules (cunning-bot bot)
             (cunning-bot sugar)
             (cunning-bot repl)
             (cunning-bot commands))

(define cunning-bot
  (bot
   #:nick "cunning-bot"
   #:server "irc.freenode.net"
   #:port 6667
   #:plugins '(shoot)
   #:commands `((flay . ,flay)
                (say-hello . ,say-hello))))

(start-repl)

;; Does not work because of bug#13018.  Fixed in trunk.  See
;; https://lists.gnu.org/archive/html/bug-guile/2013-08/msg00003.html
;;
;; (sigaction SIGINT
;;   (lambda ()
;;     (quit-irc bot))
(start-bot cunning-bot '("#cunning-bot"))
