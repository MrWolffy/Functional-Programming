#lang racket

(define (max-iter beg end tmpmax)
  (if (eq? beg end)
      (begin (display tmpmax) (newline))
      (begin (let ((t (read)))
               (if (> t tmpmax)
                   (max-iter (+ beg 1) end t)
                   (max-iter (+ beg 1) end tmpmax))))))

(define (loop beg end)
  (if (eq? beg end)
      (void)
      (begin (let ((m (read)))
               (max-iter 0 m 0))
             (loop (+ beg 1) end))))

(define (solve)
  (let ((n (read)))
    (loop 0 n)))

(solve)

  