#lang racket

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 frame)
  (car frame))

(define (edge11 frame)
  (cadr frame))

(define (edge21 frame)
  (caadr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge12 frame)
  (cadr frame))

(define (edge22 frame)
  (cddr frame))