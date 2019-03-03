#lang racket

(define (double f)
  (lambda (x) (f (f x))))

; (((double (double double)) inc) 5)
; (((double (lambda (x) (double (double x)))) inc) 5)
; (((lambda (x)
;     ((lambda (x)
;        (double (double
;                 ((lambda (x)
;                    (double (double x))))))))) inc) 5)
; 一个double加倍，四个double加16倍，返回21
