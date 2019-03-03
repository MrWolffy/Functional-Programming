#lang racket

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;(sqrt 0.00001) ;0.031356，但是(* 0.031356 0.031356) = 0.001
;(sqrt 2.4e20) ;死循环

(define (new-good-enough? guess x)
  (< (/ (abs (- (square guess) x)) guess) 0.001))

(define (new-sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x)
                     x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(new-sqrt 0.00001)
(new-sqrt 2.4e20)