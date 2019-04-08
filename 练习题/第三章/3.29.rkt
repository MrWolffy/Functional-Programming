#lang racket

(define (or-gate a1 a2 output)
  (let ((inv-a1-wire (make-wire))
        (inv-a2-wire (make-wire)))
    (let ((inv-a1 (inverter a1 inv-a1-wire))
          (inv-a2 (inverter a2 inv-a2-wire)))
      (let ((and-wire (make-wire)))
        (let ((and-inv-a1-inv-a2
               (and-gate inv-a1 inv-a2 and-wire)))
          (inverger and-inv-a1-inv-a2 output))))))

; 进行一次or运算
; 需要顺序进行inv、and、inv运算
; 所以or-gate-delay = 2 * inverter-delay + and-gate-delay