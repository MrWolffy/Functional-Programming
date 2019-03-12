#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound a) (car a))

(define (upper-bound a) (cdr a))

(define (par f r1 r2)
  (let ((p1 (f (lower-bound r1)(lower-bound r2)))
        (p2 (f (lower-bound r1)(upper-bound r2)))
        (p3 (f (upper-bound r1)(lower-bound r2)))
        (p4 (f (upper-bound r1)(upper-bound r2))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


