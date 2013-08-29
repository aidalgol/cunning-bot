(define-module (cunning-bot plugins)
  #:use-module (cunning-bot bot)
  #:export (use-plugin!))

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
  (module-use! command-module cleaned-module))
