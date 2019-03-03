#lang racket

(define (times a b)
  (cond ((= b 1) a)
        ((even? b) (times (double a) (halve b)))
        (else (+ a (times a (- b 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))
