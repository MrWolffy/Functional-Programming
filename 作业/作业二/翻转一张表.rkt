#lang racket

(define (reverse lst)
  (cond ((null? lst) '())
        (else (append
               (reverse (cdr lst))
               (list (car lst))))))

(define (solve)
  (let ((lst (read)))
    (if (eq? lst eof)
        (void)
        (begin
          (displayln (reverse lst))
          (solve)))))

(solve)