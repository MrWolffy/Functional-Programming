#lang racket

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union set1 set2)
  (cond ((null? set2) set1)
        (else (union (adjoin-set (car set2) set1)
                     (cdr set2)))))

