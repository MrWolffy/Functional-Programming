#lang racket

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x (cons 'a '()))
(define y (cons x x))
(count-pairs y) ;3

(set! x (cons 'a (cons 'b '())))
(set! y (cons x (cdr x)))
(count-pairs y) ;4

(set! x (cons 'a '()))
(set! y (cons (cons x x) (cons x x)))
(count-pairs y) ;7

; 不返回的成环即可
