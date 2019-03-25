#lang racket

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

(define (less a b)
  (cond ((null? a) #f)
        ((null? b) #t)
        ((number? a) (< a b))
        (else (let ((ca (car a)) (cb (car b)))
                (if (eq? ca cb)
                    (less (cdr a) (cdr b))
                    (less (car a) (car b)))))))
                      

(define (p a n)
  (if (= n 0) a
      (p (sort (subsets a) less) (- n 1))))

(define (unique a)
  (cond ((null? a) '())
        ((null? (cdr a)) a)
        (else (if (= (car a) (cadr a))
            (unique (cdr a))
            (cons (car a) (unique (cdr a)))))))

(define (solve)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (p (unique (sort a <)) b)) (solve)))))

(solve)