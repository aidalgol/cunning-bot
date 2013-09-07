(define-module (cunning-bot plugins misc)
  #:export (source ping))

(define (source bot sender args)
  "git clone git://github.com/aidalgol/cunning-bot.git")

(define (ping bot sender args)
  "pong")
