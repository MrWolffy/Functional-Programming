#lang racket

(define (same-parity x . lst)
  (define (helper x lst)
    (cond ((null? lst) '())
        ((= (remainder x 2)
            (remainder (car lst) 2))
         (cons (car lst) (helper x (cdr lst))))
        (else (helper x (cdr lst)))))
  (cons x (helper x lst)))
  
      