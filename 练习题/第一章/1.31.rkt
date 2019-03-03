#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (term n) n)
  (define (next n) (+ n 1))
  (new-product term 1 next n))

(define (cal-pi n)
  (define (term n) (/ (* n (+ n 2)) (* (+ n 1) (+ n 1))))
  (define (next n) (+ n 2))
  (* 4.0 (product term 2 next n)))

(define (new-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

