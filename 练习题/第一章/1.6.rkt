#lang racket

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

;将会出现死循环
;考虑下面这段代码：
;
;#lang racket
;
;(define x 0)
;(define (add)
;  (set! x (+ x 1)))
;
;(define (new-if predicate then-clause else-clause)
;  (cond (predicate then-clause)
;        (else else-clause)))
;
;(new-if (> x 0) (add) (add))
;
;(display x)
;
;输出为2，说明then-clause和else-clause都被计算，
;而特殊形式if只计算其中一个表达式
