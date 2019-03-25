#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
  (define (sum list)
  (if (null? list)
      0
      (+ (car list) (sum (cdr list)))))
  (define (unique-pairs n)
  (accumulate append
              '()
              (map (lambda (i)
                     (flatmap (lambda (j)
                            (map (lambda (k) (list i j k))
                                 (enumerate-interval (+ j 1) n)))
                          (enumerate-interval (+ i 1) n)))
                   (enumerate-interval 1 n))))
(filter (lambda (list) (= (sum list) s))
          (flatmap unique-pairs
                   (list n))))
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)