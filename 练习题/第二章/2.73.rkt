#lang racket

; (a)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; 如果exp不是一个表达式
; 就直接计算导数
; 如果exp是一个表达式
; 就采用那个表达式相应的求导法则进行求导
; 因为number和variable本身是没有tag的


; (b)
(define (install-sum-package)
  (define (first s) (car s))
  (define (second s) (cdr s))
  (define (make-sum a b) (cons a b))
  (define (deriv s) (make-sum (deriv (first s))
                              (deriv (second s))))

  (define (tag s) (attach-tag '+ s))
  (put 'first '(+) first)
  (put 'second '(+) second)
  (put 'deriv '(+) deriv)
  (put 'make-sum '+ (lambda (x y) (tag (make-sum x y))))
  'done)

(define (install-product-package)
  (define (first s) (car s))
  (define (second s) (cdr s))
  (define (make-product a b) (cons a b))
  (define (deriv s)
    (make-sum
     (make-product (first s) (deriv (second s)))
     (make-product (deriv (first s)) (second s))))

  (define (tag s) (attach-tag '* s))
  (put 'first '(*) first)
  (put 'second '(*) second)
  (put 'deriv '(*) deriv)
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done)


; (c)
(define (install-exponentiation-package)
  (define (first s) (car s))
  (define (second s) (cdr s))
  (define (make-exponentiation a b) (cons a b))
  (define (deriv s)
    (make-product
     (second s)
     (make-exponentiation (first s) (- (second s) 1))))

  (define (tag s) (attach-tag '** s))
  (put 'first '(**) first)
  (put 'second '(**) second)
  (put 'deriv '(**) deriv)
  (put 'make-exponentiation '**
       (lambda (x y) (tag (make-exponentiation x y))))
  'done)


; (d)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get (operator exp) 'deriv)) (operands exp) var)))
; 修改put的顺序即可


