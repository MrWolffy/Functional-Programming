#lang racket

(define (square a)
  (* a a))

(define (fast-exp a n)
  (cond ((= n 0) 1)
        ((= (remainder n 2) 0) (square (fast-exp a (/ n 2))))
        (else (* a (fast-exp a (- n 1))))))

(define (solve)
  (let ((a (read))
        (n (read)))
    (if (eq? a eof)
        (void)
        (begin (display (fast-exp a n))
               (newline)
               (solve)))))

(solve)