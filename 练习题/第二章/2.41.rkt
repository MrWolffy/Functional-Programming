#lang racket

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op init seqs)
  (if (null? seqs)
      init
      (op (car seqs)
          (accumulate op init (cdr seqs)))))

(define (unique-pairs n)
  (accumulate append
              '()
              (map (lambda (i)
                     (flatmap (lambda (j)
                            (map (lambda (k) (list i j k))
                                 (enumerate-interval 1 (- j 1))))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (sum list)
  (if (null? list)
      0
      (+ (car list) (sum (cdr list)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (f n s)
  (filter (lambda (list) (= (sum list) s))
          (flatmap unique-pairs
                   (list n))))

(f 10 10)