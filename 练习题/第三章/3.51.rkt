#lang racket

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (show x) (displayln x) x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

; 5
; 5
; 7
; 7
; 按理说应该在define x的时候输出0
; 在(stream-ref x 5)的时候输出123455
; 在(stream-ref x 7)的时候输出677