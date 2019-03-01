#lang racket

(define (f a b c)
  (define tmpmax (+ a b))
  (if (> (+ b c) tmpmax)
      (set! tmpmax (+ b c))
      (void))
  (if (> (+ a c) tmpmax)
      (set! tmpmax (+ a c))
      (void))
  tmpmax)

(f 0 1 2)
(f 1 2 0)
(f 2 0 1)
