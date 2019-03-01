#lang racket

(define v (make-vector 110 0))
(define (ans)
  (define (read-vec vec l)
    (let ((a (read)))
      (if (eq? a eof)
          l
          (begin (vector-set! vec l a)
               (read-vec vec (+ l 1))))))
  
  (define (bubble-sort vec l i)
    (define (bubble-sort-iter vec l i j)
      (if (>= j l)
          (void)
          (begin (if (> (vector-ref vec i)
                        (vector-ref vec j))
                     (let ((t (vector-ref vec i)))
                       (begin (vector-set! vec i (vector-ref vec j))
                              (vector-set! vec j t)))
                     (void))
                 (bubble-sort-iter vec l i (+ j 1)))))

    (if (>= i l)
        (void)
        (begin (bubble-sort-iter vec l i (+ i 1))
               (bubble-sort vec l (+ i 1)))))
    
  (define (print-vec vec l it)
    (if (>= it l)
        (void)
        (begin (if (and (> it 0) (eq? (vector-ref vec (- it 1))
                                      (vector-ref vec it)))
                   (void)
                   (begin (display (vector-ref vec it))
                          (display " ")))
               (print-vec vec l (+ it 1)))))

  (define len (read-vec v 0))
  (bubble-sort v len 0)
  (print-vec v len 0))


(ans)