#lang racket

(define (deep-reverse lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) lst)
        (else (append (deep-reverse (cdr lst))
                      (list (deep-reverse (car lst)))))))

