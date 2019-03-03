#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x h))
  (define (add-2h x) (+ x h h))
  (* (/ h 3.0)
        (+ (sum f a add-h b)
           (sum f (add-h a) add-h (- b h))
           (* 2 (sum f (+ a h) add-2h (- b h))))))

(define (cube x)
  (* x x x))

(integral cube 0 1 0.01)
(simpson cube 0 1 100)
(integral cube 0 1 0.001)
(simpson cube 0 1 1000)