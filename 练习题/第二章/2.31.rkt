#lang racket

(define (tree-map op tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (op tree))
        (else (cons (tree-map op (car tree))
                    (tree-map op (cdr tree))))))

(define (square-tree tree)
  (define (square x) (* x x))
  (tree-map square tree))