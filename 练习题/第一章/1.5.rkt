#lang racket

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;正则序求值返回0，应用序求值死循环
;正则序：(test 0 (p)) -> (if (= 0 0) 0 (p))) -> 0
;应用序：(test 0 (p)) -> (test 0 (p))
