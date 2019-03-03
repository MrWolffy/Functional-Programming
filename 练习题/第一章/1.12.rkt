#lang racket

(define (print-vec vec beg end)
  (if (>= beg end)
      (newline)
      (begin (display (vector-ref vec beg))
             (display " ")
             (print-vec vec (+ beg 1) end))))

(define (pascal-iter beg end tmpvec)
  (define newvec (make-vector (+ beg 2) 0))
  (vector-set! newvec 0 1)
  (vector-set! newvec (- (vector-length newvec) 1) 1)
  (if (>= beg end)
      (void)
      (begin (print-vec tmpvec 0 (+ beg 1))
             (pascal-iter (+ beg 1) end
                          (pascal-inner-loop 1 (+ beg 1) tmpvec newvec)))))

(define (pascal-inner-loop beg end oldvec newvec)
  (if (eq? beg end)
      newvec
      (begin (vector-set! newvec beg (+ (vector-ref oldvec (- beg 1))
                                        (vector-ref oldvec beg)))
             (pascal-inner-loop (+ beg 1) end oldvec newvec))))

(define (pascal n)
  (pascal-iter 0 n (vector 1)))
