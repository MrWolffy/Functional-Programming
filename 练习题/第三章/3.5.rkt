#lang racket

(define (random-in-range low high) 
   (let ([range (- high low)]) 
     (+ low (* (random) range)))) 

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (monte-carlo trials p) 1.0
     (abs (- x1 x2))
     (abs (- y1 y2))))

(define (estimate-pi trials)
  (define (square x) (* x x))
  (estimate-integral
   (lambda ()
     (< (+ (square (random-in-range -1 1))
           (square (random-in-range -1 1))) 1))
   -1 1
   -1 1
   trials))

(estimate-pi 10000)
