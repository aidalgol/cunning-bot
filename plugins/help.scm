(define-module (cunning-bot plugins help)
  #:use-module (ice-9 documentation)
  #:use-module (cunning-bot bot)
  #:export (help))

(define bot-command (@@ (cunning-bot bot) bot-command))

(define (help bot sender args)
  "help COMMAND : returns the documentation for COMMAND"
  (define command-name
    (string->symbol (string-trim args)))
  (cond ((bot-command bot command-name) =>
         (lambda (var)
           (or (object-documentation var)
               (format #f "no help for command: ~a" command-name))))
        (else (format #f "no such command: ~a" command-name))))
