#lang racket

(define (fast-expr a n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((= (remainder n 2) 0) (fast-expr (square a) (/ n 2)))
        (else (* a (fast-expr a (- n 1))))))

(define (cons x y)
  (* (fast-expr 2 x) (fast-expr 3 y)))

(define (car x)
  (cond ((not (= (remainder x 2) 0)) 0)
        (else (+ 1 (car (/ x 2))))))

(define (cdr x)
  (cond ((not (= (remainder x 3) 0)) 0)
        (else (+ 1 (cdr (/ x 3))))))