#lang racket

(define (count-frac n d k)
  (define (count-frac-iter n d k count)
    (if (> count k)
        1
        (/ (n k) (+ (d k)
                    (count-frac-iter n d k (+ count 1))))))
  (count-frac-iter n d k 1))

(define (new-count-frac n d k)
  (define (new-count-frac-iter n d k result)
    (if (= k 0)
        result
        (new-count-frac-iter n d (- k 1)
                             (/ (n k) (+ (d k) result)))))
  (new-count-frac-iter n d k 0))