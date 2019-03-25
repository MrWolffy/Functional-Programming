#lang racket

(define (list-set! lst i val j)
  (if (null? lst)
      '()
      (cons (if (= i j) val (car lst))
            (list-set! (cdr lst) i val (+ j 1)))))

(define (licc a lics i j)
  (if (= j i)
      lics
      (begin 
             (licc a (if (and (> (list-ref a i) (list-ref a j))
                      (<= (list-ref lics i) (list-ref lics j)))
                 (list-set! lics i (+ 1 (list-ref lics j)) 0)
                 lics) i (+ j 1)))))

(define (lic a lics i)
  (if (= i (length a))
      lics
      (lic a (licc a lics i 0) (+ i 1))))

(define (findmax lst)
  (if (null? lst)
      0
      (max (findmax (cdr lst))
           (car lst))))

(define (myloop)
  (let ((a (read)))
    (define lics (make-list 110 1))
    (if (eq? a eof)
        (void)
        (begin (display (findmax (lic a lics 0))) (newline)
               (myloop)))))

(myloop)