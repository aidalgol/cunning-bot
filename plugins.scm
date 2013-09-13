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
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-1) #:select (alist-cons remove))
  #:use-module (cunning-bot bot)
  #:export (use-plugin!
            use-plugins!))


(define-record-type plugin
  (%make-plugin module interface setup teardown)
  plugin?
  (module plugin-module)
  (interface plugin-interface)
  (setup plugin-setup-procedure)
  (teardown plugin-teardown-procedure))

(define (lookup-plugin plugin-name)
  (define module
    (resolve-module `(cunning-bot plugins ,plugin-name)))
  (define hidden-exports
    (filter (lambda (f)
              (module-defined? module f))
            magic-exports))
  (define public-interface
    (resolve-interface `(cunning-bot plugins ,plugin-name) #:hide hidden-exports))
  (define setup! (module-ref module 'setup! #f))
  (define teardown! (module-ref module 'teardown! #f))
  (%make-plugin module public-interface setup! teardown!))

;; Setup should take additional arguments
(define magic-exports '(setup! teardown!))

(define (use-plugin! bot plugin-name)
  "Loads plugin (cunning-bot plugins PLUGIN-NAME) into BOT"
  ;; TODO: check plugin not already loaded
  (define plugin (lookup-plugin plugin-name))
  (define command-module
    ((@@ (cunning-bot bot) get-commands) bot))
  (module-use! command-module (plugin-interface plugin))
  (and=> (plugin-setup-procedure plugin)
         (lambda (f) (f bot)))
  (and=> (plugin-teardown-procedure plugin)
         (lambda (f) (add-quit-hook! bot f)))
  (bot-plugins-set! bot (alist-cons plugin-name plugin (bot-plugins bot))))

(define (use-plugins! bot plugins)
  (for-each (lambda (plugin)
              (use-plugin! bot plugin))
            plugins))
