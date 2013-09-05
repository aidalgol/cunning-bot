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
