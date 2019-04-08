#lang racket


(define (averager a b c)
  (let ((cons (make-connector))
        (sum (make-connector)))
    (constant 0.5 cons)
    (adder a b sum)
    (multiplier sum constant c)))