#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x)))))
; (lambda (f) (lambda (x) (f ((lambda (x) x)
;                             x))))
; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x)))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

; 这个我不会，抄的csdn，看到答案之后觉得好有道理
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

