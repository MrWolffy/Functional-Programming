#lang racket

(define (fast-times-iter a b result)
  (cond ((= b 0) result)
        ((even? b) (fast-times-iter (double a) (halve b) result))
        (else (fast-times-iter a (- b 1) (+ result a)))))

(define (fast-times a b)
  (fast-times-iter a b 0))

(define (even? n)
  (= (remainder n 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))
