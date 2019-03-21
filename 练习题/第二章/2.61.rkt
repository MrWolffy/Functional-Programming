#lang racket

(define (adjoin-set x set)
  (cond ((null? set) '())
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


; 如果x比集合头部元素小，说明集合中根本没有这个元素

