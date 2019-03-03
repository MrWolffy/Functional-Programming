#lang racket

(define (filtered-accumulate filter combiner null-value
                             term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a)
           (iter (next a) (combiner (term a) result)))
          (else
           (iter (next a) result))))
  (iter a null-value))

(define (sum-prime a b)
  (define (term n) n)
  (define (next n) (+ n 1))
  (filtered-accumulate prime? + 0 term a next b))

(define (my-product n)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (filter 1)
    (= (gcd i n) i))
  (define (term n) n)
  (define (next n) (+ n 1))
  (filtered-accumulate filter * 1 term 2 next (- n 1)))


  
