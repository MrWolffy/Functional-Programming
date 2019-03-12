#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound a) (car a))

(define (upper-bound a) (cdr a))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i)) (* 2 (center i))))