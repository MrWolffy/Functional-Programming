#lang racket

(define (square a)
  (* a a))

(define (fast-exp a n)
  (cond ((= n 0) 1)
        ((= (remainder n 2) 0) (square (fast-exp a (/ n 2))))
        (else (* a (fast-exp a (- n 1))))))

(define (cont-frac-iter n d k)
  (define (count-frac-iter n d k result)
    (if (= k 0)
        result
        (count-frac-iter n d (- k 1)
                         (/ (n k) (+ (d k) result)))))
  (count-frac-iter n d k 0))

(define (cal-e k)
  (define (d i)
    (let ((t (remainder i 3)))
      (cond ((or (= t 1) (= t 0)) 1)
            (else (fast-exp 2 (/ (+ i 1) 3))))))
  (+ (cont-frac-iter (lambda (i) 1.0) d k) 2))



