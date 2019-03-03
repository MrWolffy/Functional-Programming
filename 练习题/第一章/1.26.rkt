#lang racket

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; 如果采用应用序求值，本来当偶数的时候expmod只需要做一遍
; 但是如果这样写就要做两遍了

