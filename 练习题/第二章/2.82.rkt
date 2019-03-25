#lang racket

; 只能写出强转第一个参数类型的代码
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t2->t1 (get-coercion type2 type1)))
                  (if t2->t1
                      (apply-generic
                       op
                       (apply-generic a1 (t2->t1 a2))
                       (cddr args))
                      (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags))))))

; 