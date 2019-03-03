#lang racket

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (fast-prime? n 10))

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
; 1009 *** 0.012939453125
; 1013 *** 0.023193359375
; 1019 *** 0.013916015625
(search-for-primes 10000 10038)
; 10007 *** 0.01904296875
; 10009 *** 0.017822265625
; 10037 *** 0.01806640625
(search-for-primes 100000 100044)
; 100003 *** 0.02001953125
; 100019 *** 0.02099609375
; 100043 *** 0.05712890625
(search-for-primes 1000000 1000038)
; 1000003 *** 0.030029296875
; 1000033 *** 0.024169921875
; 1000037 *** 0.024169921875

; 对接近1000000的素数检查的时间大约是对接近1000的素数检查的时间的2倍