#lang racket

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor divisor)
  (if (= divisor 2)
      3
      (+ divisor 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (void)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes beg end)
  (if (>= beg end)
      (void)
      (begin (timed-prime-test beg)
             (search-for-primes (+ beg 1) end))))

(search-for-primes 1000 1020)
; 1009 *** 0.001953125
; 1013 *** 0.0009765625
; 1019 *** 0.001953125
(search-for-primes 10000 10038)
; 10007 *** 0.0048828125
; 10009 *** 0.005126953125
; 10037 *** 0.0048828125
(search-for-primes 100000 100044)
; 100003 *** 0.01318359375
; 100019 *** 0.012939453125
; 100043 *** 0.01220703125
(search-for-primes 1000000 1000038)
; 1000003 *** 0.0390625
; 1000033 *** 0.0390625
; 1000037 *** 0.0390625

; n较大时比值约为1.6
; 调用next-divisor函数需要的时间比+运算的时间更长
