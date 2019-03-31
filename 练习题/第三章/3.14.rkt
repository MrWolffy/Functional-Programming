#lang racket
(require r5rs)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; mystery会把列表翻转

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
; '(d c b a)