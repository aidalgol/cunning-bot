(define-module (cunning-bot plugins ddg)
  #:use-module (ddg)
  #:export ((ddg-define . define)))

(define (one-word? s)
  (not (string-index s char-whitespace?)))

(define (ddg-define bot sender args)
  (define args* (string-trim-both args))
  (if (not (one-word? args*))
      (string-append sender ": One word at a time, please.")
      (let* ((data (zero-click (string-append "!define " args*)))
             (definition (hash-ref data "Definition"))
             (url-string (hash-ref data "DefinitionURL")))
        (cond ((string-null? definition)
               (format #f "A definition for ~a is not available" args*))
              ((string-null? url-string) definition)
              (else
               (format #f "~a\nDefinition courtesy of ~a"
                       definition url-string))))))
