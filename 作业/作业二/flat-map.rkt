#lang racket

(define (flat new-lst lst)
  (cond ((null? lst) new-lst)
        ((not (pair? lst))
         (append new-lst (list lst)))
        (else (append
               (flat new-lst (car lst))
               (flat new-lst (cdr lst))))))
         

(define (solve)
  (let ((lst (read)))
    (if (eq? lst eof)
        (void)
        (begin
          (displayln (flat '() lst))
          (solve)))))

(solve)