#lang racket

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carmichael-iter a n flag)
  (cond ((= a n) flag)
        ((eq? flag false) false)
        (else
         (carmichael-iter (+ a 1)
                          n
                          (= (expmod a n n)
                             (remainder a n))))))

(define (carmichael n)
  (carmichael-iter 2 n true))

(carmichael 561)
(carmichael 1105)
(carmichael 1729)
(carmichael 2465)
(carmichael 2821)
(carmichael 6601)




