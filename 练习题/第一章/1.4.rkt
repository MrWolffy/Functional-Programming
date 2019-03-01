#lang racket

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; if b > 0: return a + b
; else: return a - b

; return a + abs(b)
