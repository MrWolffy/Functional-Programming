#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (make-sum a1 . a2)
  (cond ((null? (car a2)) a1)
        ((=number? (make-sum (car a2) (cdr a2)) 0) a1)
        ((=number? a1 0) (make-sum (car a2) (cdr a2)))
        ((and (number? a1) (number? (car a2)))
         (make-sum (+ a1 (car a2)) (cdr a2)))
        ((sum? (car a2)) (append (list '+ a1) (cadr a2)))
        (else (append (list '+ a1) (list (car a2))))))

(define (make-product m1 . m2)
  (cond ((null? (car m2)) m1)
        ((or (=number? m1 0) (=number? (car m2) 0)) 0)
        ((=number? m1 1) (make-product (car m2) (cdr m2)))
        ((=number? (make-product (car m2) (cdr m2)) 1) m1)
        ((product? m2) (append (list '* m1) (cadr m2)))
        (else (append (list '* m1) (list (car m2))))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (cond ((pair? (cdddr s)) (cons '+ (cddr s)))
        (else (caddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (cond ((pair? (cdddr p)) (cons '* (cddr p)))
        (else (caddr p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(* x y (+ x 3)) 'x)