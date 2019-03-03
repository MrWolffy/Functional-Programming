#lang racket

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (new-f n)
  (define (new-f-iter a b c n)
    (if (= n 0)
        a
        (new-f-iter b
                    c
                    (+ c (* 2 b) (* 3 a))
                    (- n 1))))
  (new-f-iter 0 1 2 n))

; (f 30)与(new-f 30)差距明显
