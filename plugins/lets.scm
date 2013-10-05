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

(define-module (cunning-bot plugins lets)
  #:use-module (srfi srfi-9)
  #:use-module (cunning-bot bot)
  #:use-module (ice-9 regex)
  #:export (setup! teardown! lets))

(define-record-type corpus
  (%make-corpus size vector filename)
  corpus?
  (size corpus-size corpus-size-set!)
  (vector corpus-vector corpus-vector-set!)
  (filename corpus-filename))

(define random-state (random-state-from-platform))

(define* (make-corpus #:optional (filename #f))
  (%make-corpus 0 #() filename))

(define (file->corpus filename)
  (call-with-input-file filename
    (lambda (in)
      (define vec (read in))
      (if (eof-object? vec)
          (make-corpus filename)
          (%make-corpus (vector-length vec)
                        vec
                        filename)))))

(define (save-corpus! corpus)
  (call-with-output-file (corpus-filename corpus)
    (lambda (out)
      (write (corpus-vector corpus) out))))

(define (add-to-corpus! corpus item)
  ;; Very inefficient, but it won't be common, hopefully.
  (define (vector-cons item vec)
    (list->vector (cons item (vector->list vec))))
  (corpus-vector-set! corpus (vector-cons item (corpus-vector corpus))))


(define *corpus*
  (%make-corpus 1 #("let's add entries to the corpus!") #f))

(define (random-lets)
  (define size (corpus-size *corpus*))
  (vector-ref (corpus-vector *corpus*) (random size random-state)))

(define lets-string?
  (let ((rx (make-regexp "^ *let'?s " regexp/icase)))
    (lambda (message)
      (regexp-exec rx message))))

(define (snarf-lets bot sender target message ctcp)
  (when (lets-string? message)
    (add-to-corpus! *corpus* message)))

(define *lets-file* "/tmp/lets")

(define* (setup! bot #:optional (lets-file *lets-file*))
  (set! *corpus*
        (catch 'system-error
          (lambda ()
            (file->corpus lets-file))
          (lambda args
            (if (= ENOENT (system-error-errno args))
                (make-corpus lets-file)  ; if it doesn't exist, make it
                (apply throw args)))))
  (add-privmsg-hook! bot snarf-lets))

(define (teardown! bot)
  (remove-privmsg-hook! bot snarf-lets)
  (save-corpus! *corpus*))

(define (lets bot sender args)
  (random-lets))
