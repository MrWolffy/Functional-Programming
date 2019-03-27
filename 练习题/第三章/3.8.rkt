#lang racket

(define n 0)

(define (helper x)
    (if (= n x)
        x
        (let ((tmp n))
          (set! n x)
          tmp)))

(define (f n)
  (helper n))

(+ (f 0) (f 1))
