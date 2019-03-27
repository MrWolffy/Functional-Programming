#lang racket

(define (make-accumulator init)
  (let ((amount init))
    (lambda (x)
      (begin (set! init (+ init x)) init))))