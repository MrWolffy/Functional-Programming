#lang racket

(define (reverse lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) lst)
        (else (append
               (reverse (cdr lst))
               (list (reverse (car lst)))
               ))))

(define (solve)
  (let ((lst (read)))
    (if (eq? lst eof)
        (void)
        (begin
          (displayln (reverse lst))
          (solve)))))

(solve)