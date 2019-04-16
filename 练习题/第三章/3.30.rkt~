#lang racket

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (ripple-carry-adder a b s c-out)
  (define (ripple-carry-adder-helper a1 b1 s1 c-in c-out)
    (let ((tmp-c-out (make-wire)))
      (if (not (null? (cdr a1)))
          (begin
            (ripple-carry-adder-helper
             (cdr a1) (cdr b1) (cdr s1) c-in tmp-c-out)
            (full-adder
             (car a1) (car b1) tmp-c-out (car s1) c-out))
          (full-adder a1 b1 c-in s1 tmp-c-out))))
  (let ((c-in (make-wire)))
    (set-signal! c-in 0)
    (ripple-carry-adder-helper a b s c-in (make-wire))))
    
    
; 这是一个递归的过程
; 每一次递归都要等待一个全加器完成计算
; 一个全加器需要等待一个半加器的计算时间和一个或门的计算时间
; 一个半加器的计算时间是 max(and-gate-delay, or-gate-delay) + inverter-delay + and-gate-delay
; 所以总的等待时间为: n * (max(and-gate-delay, or-gate-delay) + inverter-delay + and-gate-delay + or-gate-delay)
