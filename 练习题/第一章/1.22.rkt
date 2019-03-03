#lang racket

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

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
; 1013 *** 0.00390625
; 1019 *** 0.001953125
(search-for-primes 10000 10038)
; 10007 *** 0.007080078125
; 10009 *** 0.007080078125
; 10037 *** 0.007080078125
(search-for-primes 100000 100044)
; 100003 *** 0.02001953125
; 100019 *** 0.02001953125
; 100043 *** 0.02001953125
(search-for-primes 1000000 1000038)
; 1000003 *** 0.06298828125
; 1000033 *** 0.06298828125
; 1000037 *** 0.06298828125

; 大致服从O(n^1/2)增长趋势