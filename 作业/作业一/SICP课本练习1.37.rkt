#lang racket
(define (cont-frac-iter N D k)
  (define (count-frac-iter n d k result)
    (if (= k 0)
        result
        (count-frac-iter n d (- k 1)
                         (/ (n k) (+ (d k) result)))))
  (count-frac-iter N D k 0))

(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)



