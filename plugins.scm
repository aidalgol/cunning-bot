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

(define-module (cunning-bot plugins)
  #:use-module (cunning-bot bot)
  #:export (use-plugin!
            use-plugins!))

(define magic-exports '(setup! teardown!))

(define (use-plugin! bot plugin-name)
  (define plugin-module (resolve-module `(cunning-bot plugins ,plugin-name)))
  (define hidden-exports
    (filter (lambda (f)
              (module-defined? plugin-module f))
            magic-exports))
  (define cleaned-module
    (resolve-interface `(cunning-bot plugins ,plugin-name) #:hide hidden-exports))
  (define command-module
    ((@@ (cunning-bot bot) get-commands) bot))
  (module-use! command-module cleaned-module)
  (when (module-defined? plugin-module 'setup!)
    ((module-ref plugin-module 'setup!)))
  (when (module-defined? plugin-module 'teardown!)
    (add-quit-hook! bot (module-ref plugin-module 'teardown!))))

(define (use-plugins! bot plugins)
  (for-each (lambda (plugin)
              (use-plugin! bot plugin))
            plugins))
