#lang racket

; (a)
(define (get-record x)
  (apply-generic 'get-record x))
; 分支机构的文件应当具有自己的get-record


; (b)
(define (get-salary x)
  (apply-generic 
  	'salary 
  	(apply-generic 'get-record x)))
; 每个分支机构应当能利用salary过程找出自己雇员的薪水

; (c)
(define (find-employee-record name lst)
  (if (null? lst)
      '()
      (let ((result (get-record (car lst))))
        (if (eq? name
                 ((apply-generic 'name (type-tag result))
                  (contents result)))
            (cons
             (contents result)
             (find-employee-record name (cdr lst)))
            (find-employee-record name (cdr lst))))))

; (d)
; 在表中增加一列新公司的过程即可


