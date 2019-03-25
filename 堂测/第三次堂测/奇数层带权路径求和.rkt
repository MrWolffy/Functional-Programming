#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (treesum lst layer)
  (cond ((null? lst) 0)
        ((not (pair? lst)) (if (even? layer) 0 (* lst layer)))
        (else (+ (if (pair? (car lst))
                     (treesum (car lst) (+ layer 1))
                     (treesum (car lst) layer))
                 (treesum (cdr lst) layer)))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (treesum a 1)) (newline)
               (myloop)))))

(myloop)