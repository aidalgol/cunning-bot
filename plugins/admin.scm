(define-module (cunning-bot plugins admin)
  #:use-module (ice-9 match)
  #:export (debug auth revoke))

;; Should be bot local
(define *master* #f)

(define (is-master? s)
  (and *master* (string-ci=? s *master*)))

(define (fail-if-not-master s)
  (or (is-master? s)
      ;;(throw 'auth-fail "You need to be my master to do this")
      (error "You need to be my master to do this" s)))

(define *auth-state* #f)

(define (is-auth? s)
  (and *auth-state* (string-ci=? s *auth-state*)))

(define hex-chars "0123456789abcdef")

(define (new-auth-state)
  (define s
    (string-tabulate (lambda (_)
                       (string-ref hex-chars (random 16)))
                     16))
  (set! *auth-state* s)
  s)

(define (auth bot sender args)
  (match (string-tokenize args)
    (()
     (format #t "new auth string:~a~%" (new-auth-state))
     "New authentication string created. Check stdout.")
    ((str)
     (cond ((is-auth? str)
            (set! *master* sender)
            (set! *auth-state* #f)
            "welcome, my master")
           (else
            (format #t "new auth string:~a~%" (new-auth-state))
            "Authentication failed. New auth string created.")))
    (_ "command expects only one arg")))

(define (revoke bot sender args)
  (fail-if-not-master sender)
  (set! *master* #f)
  "done.")

(define (set-debugging! val)
  (set! (@@ (cunning-bot bot) debugging) val))

(define (toggle-debugging!)
  (set! (@@ (cunning-bot bot) debugging)
        (not (@@ (cunning-bot bot) debugging))))

(define (debug bot sender args)
  (fail-if-not-master sender)
  (match (string-downcase (string-trim-both args))
    ("on" (set-debugging! #t) "done")
    ("off" (set-debugging! #f) "done")
    ("" (toggle-debugging!) "toggled")
    (other (string-append "option not supported: " other))))
