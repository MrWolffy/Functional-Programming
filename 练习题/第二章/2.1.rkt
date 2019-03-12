#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (sgn (if (< (* n d) 0) -1 1)))
    (cons (* sgn (abs (/ n g))) (abs (/ d g)))))

(make-rat 2 1)
(make-rat -2 1)
(make-rat 2 -1)