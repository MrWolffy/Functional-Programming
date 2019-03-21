#lang racket

(define (make-segment vect1 vect2)
  (cons vect1 vect2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))