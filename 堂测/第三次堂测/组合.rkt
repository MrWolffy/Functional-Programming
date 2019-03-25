#lang racket

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

(define (filter predicate sequence) (cond ((null? sequence) '())
          ((predicate (car sequence))
           (cons (car sequence)
(filter predicate (cdr sequence)))) (else (filter predicate (cdr sequence)))))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (reverse 
                (filter
                 (lambda (x) (= (length x) a))
                 (subsets b)))) (newline)
               (myloop)))))

(myloop)