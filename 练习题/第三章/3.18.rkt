#lang racket

(define (circle? x)
    (inner x '()))

(define (inner x memo-list)
    (cond ((null? x) #f)
          ((not (pair? x)) #f)
          ((false? (memq x memo-list))
           (inner (cdr x) (cons x memo-list)))
          (else #t)))

