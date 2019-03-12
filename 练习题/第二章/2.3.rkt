#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-rectangle height-seg length-seg)
  (cons height-seg length-seg))

(define (get-height-seg rec)
  (car rec))

(define (get-length-seg rec)
  (cdr rec))

(define (get-height rec)
  (let ((height (get-height-seg rec)))
    (abs (- (y-point (start-segment height))
            (y-point (end-segment height))))))

(define (get-length rec)
  (let ((length (get-length-seg rec)))
    (abs (- (x-point (start-segment length))
            (x-point (end-segment length))))))

(define (area rec)
  (* (get-height rec) (get-length rec)))



(define sega (make-segment
              (make-point 0 0)
              (make-point 0 4)))
(define segb (make-segment
              (make-point 0 0)
              (make-point 5 0)))
(define rec (make-rectangle sega segb))
(area rec)