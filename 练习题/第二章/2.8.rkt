#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound a) (car a))

(define (upper-bound a) (cdr a))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))