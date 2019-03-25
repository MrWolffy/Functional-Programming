#lang racket

(define (rc a b)
  (cond ((null? b) a)
        ((null? a) '())
        ((< (car a) (car b))
         (cons (car a)
               (rc (cdr a) b)))
        ((= (car a) (car b))
         (rc (cdr a) (cdr b)))
        (else (rc a (cdr b)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2))))
                 (else
                  (cons x1 (union-set (cdr set1) (cdr set2)))))))))

(define (sd a b)
  (union-set (rc a b) (rc b a)))

(define (unique set)
  (cond ((null? set) '())
        ((null? (cdr set)) (list (car set)))
        ((= (car set) (cadr set)) (unique (cdr set)))
        (else (cons (car set) (unique (cdr set))))))
      

(define (solve)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (let ((sa (unique (sort a <)))
              (sb (unique (sort b <))))
          (begin (display (rc sa sb))
                 (displayln (sd sa sb))
                 (solve))))))

(solve)