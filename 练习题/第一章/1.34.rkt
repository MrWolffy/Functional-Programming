#lang racket

(define (f g)
  (g 2))

(f f) 
; error: not a procedure
; 根据定义f是一个值
; 但g是一个过程
; (f f) -> (f 2) -> (2 2)