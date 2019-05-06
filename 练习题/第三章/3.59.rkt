#lang racket

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integer (integers-starting-from 1))

(define (intergrate-series a)
  (stream-map * a (stream-map (lambda (x) (/ 1 x)) integer)))

(define cosine-series (stream-cons 1 sine-series))
(define sine-series (stream-cons 0 (stream-map - cosine-series)))