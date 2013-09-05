(define-module (cunning-bot plugins shoot)
  #:use-module (cunning-bot bot)
  #:export (shoot))

(define firearms
  #(("shotgun" . "blasts")
    ("hose" . "drenches")))

(define (random-firearm)
  (vector-ref firearms (random (vector-length firearms))))

(define (shoot bot sender args)
  (define firearm (random-firearm))
  (make-action (format #f "loads its ~a and ~a ~a"
                       (car firearm)
                       (cdr firearm)
                       args)))
