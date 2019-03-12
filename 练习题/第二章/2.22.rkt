#lang racket

(define (square x) (* x x))

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer)))) 
                    ;每次cons的时候都把后算出的结果放到前面了
  (iter items '()))

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  ;表的结构不对
  (iter items '()))