#lang racket

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; 可能发生溢出，而且大整数取模不一定是O(1)的

