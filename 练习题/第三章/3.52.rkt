#lang racket

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (display-stream s)
  (stream-for-each displayln s))

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
; sum = 1
(define y (stream-filter even? seq))
; sum = 6
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
; sum = 10
(stream-ref y 7)
; 136
; sum = 136
(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; sum = 210
; 如果不使用优化
; 那么每次sum都需要从头开始求值s