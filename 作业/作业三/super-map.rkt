#lang racket
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (map op (cdr lst)))))
  
(define (super-map op . w)
  (define (helper result op w)
    (if (null? (car w))
        result
        (helper (cons (apply op (map
                         (lambda (x) (car x))
                         w)) result)
                op
                (map
                     (lambda (x) (cdr x))
                     w))))
  (reverse (helper '() op w)))
(define (myloop)
  (let ((a (read))
        (b (read))
        (c (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (super-map + a b c)) 
               (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
               (myloop)))))
(myloop)