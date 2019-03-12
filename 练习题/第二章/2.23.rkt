#lang racket

(define (for-each op lst)
  (if (null? lst)
      (void)
      (begin (op (car lst))
             (for-each op (cdr lst)))))