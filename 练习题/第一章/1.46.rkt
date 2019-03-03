#lang racket

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (try next))))
    (try x)))

(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- guess x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f guess)
  (define (good-enough? x guess)
    (< (abs (- x guess)) 0.00001))
  ((iterative-improve good-enough? f) guess))
