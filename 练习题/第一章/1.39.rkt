#lang racket

(define (square a)
  (* a a))

(define (tan-cf x k)
  (define (n k x)
    (if (= k 1) x (square x)))
  (define (d k)
    (- (* 2 k) 1))
  (define (tan-cf-iter x k result)
    (if (= k 0)
        result
        (tan-cf-iter x (- k 1)
                     (/ (n k x) (- (d k) result)))))
  (tan-cf-iter x k 0.0))

