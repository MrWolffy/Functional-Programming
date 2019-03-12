#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound a) (car a))

(define (upper-bound a) (cdr a))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i)) (* 2 (center i))))


(define a (make-center-percent 1 0.01))
(define b (make-center-percent 10 0.001))
(display (center (par1 a a)))
(display ",")
(display (percent (par1 a a)))
(newline)
(display (center (par2 a a)))
(display ",")
(display (percent (par2 a a)))
(newline)
(display (center (par1 a b)))
(display ",")
(display (percent (par1 a b)))
(newline)
(display (center (par2 a b)))
(display ",")
(display (percent (par2 a b)))
(newline)