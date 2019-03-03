#lang racket

(define (square x)
  (* x x))

(define (non-trivial root n)
  (and (= (remainder (square root) n) 1)
       (not (= root 1))
       (not (= root (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((t (expmod base (/ exp 2) m)))
           (if (non-trivial t m)
            0
            (remainder (square t) m))))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 2 10)
(fast-prime? 3 10)
(fast-prime? 4 10)
(fast-prime? 5 10)
(fast-prime? 6 10)

(newline)

(fast-prime? 561 10)
(fast-prime? 1105 10)
(fast-prime? 1729 10)
(fast-prime? 2465 10)
(fast-prime? 2821 10)
(fast-prime? 6601 10)
