#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (if (= n 0)
      '()
      (set!
       cont-list
       (cons
        (call/cc (lambda (m) (f (read)) m))
        (set-cont-list (- n 1))))))

(define (f x)
  (if (= x 0)
      (void)
      (begin (displayln x)
             (f (- x 1)))))
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)