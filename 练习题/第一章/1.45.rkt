#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (average-damp f)
  (lambda(x) (average x (f x))))

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x)
  (* x x))

(define (nth-root x n)
  (define (logt n a)
    (if (or (< (fast-expt 2 a) n) (= (fast-expt 2 a) n))
        (logt n (+ a 1))
        (- a 1)))
  (fixed-point
   (repeated
    (average-damp
     (lambda (y) (/ x (fast-expt y (- n 1)))))
    (logt n 1))
   1.0))

