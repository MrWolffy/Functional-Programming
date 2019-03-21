#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (operation exp)
  (if (memq '+ exp) '+ '*))

(define (make-sum a1 a2)
  (define (qualify? a)
    (or (pair? a) (symbol? a)))
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (qualify? a1) (qualify? a2))
         (cond ((and (pair? a1) (pair? a2)) (append a1 (list '+) a2))
            ((pair? a1) (append a1 (list '+ a2)))
            ((pair? a2) (append (list a1 '+) a2))
            (else (list a1 '+ a2))))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (eq? '+ (operation x)))

(define (addend expr) 
   (define (iter expr result) 
         (if (eq? (car expr) '+) 
           result 
           (iter (cdr expr) (append result (list (car expr)))))) 
   (let ((result (iter expr '()))) 
     (if (= (length result) 1) 
         (car result) 
         result)))

 (define (augend expr) 
   (let ((result (cdr (memq '+ expr)))) 
     (if (= (length result) 1) 
         (car result) 
         result))) 

 (define (product? expr) 
   (eq? '* (operation expr))) 
 (define (multiplier expr) 
   (define (iter expr result) 
         (if (eq? (car expr) '*) 
           result 
           (iter (cdr expr) (append result (list (car expr)))))) 
   (let ((result (iter expr '()))) 
     (if (= (length result) 1) 
         (car result) 
         result)))

 (define (multiplicand expr) 
   (let ((result (cdr (memq '* expr)))) 
     (if (= (length result) 1) 
         (car result) 
         result))) 

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

