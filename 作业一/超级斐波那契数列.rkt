#lang racket

(define (super-fib beg end vec)
  (if (eq? beg end)
      vec
      (begin (vector-set! vec beg
                          (+ (vector-ref vec (- beg 1))
                             (* (vector-ref vec (- beg 2)) 4)
                             (* (vector-ref vec (- beg 3)) 5)
                             (* (vector-ref vec (- beg 4))
                                (vector-ref vec (- beg 4))
                                -2)
                             (* (vector-ref vec (- beg 5))
                                (vector-ref vec (- beg 5))
                                (vector-ref vec (- beg 5)))))
             (super-fib (+ beg 1) end vec))))
  

(define (solve vec)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (vector-ref vec a))
               (newline)
               (solve vec)))))

(solve (super-fib 5 60 (make-vector 100 1)))