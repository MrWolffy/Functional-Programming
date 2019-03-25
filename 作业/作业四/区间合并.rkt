#lang racket


(define (merge set)
  (cond ((null? set) '())
        ((null? (cdr set)) set)
        (else (let ((a (car set))
                    (b (cadr set)))
                (if (< (cadr a) (car b))
                    (cons a (merge (cdr set)))
                    (merge (cons (merge-one (car set)
                                      (cadr set))
                           (cddr set))))))))

(define (merge-one a b)
  (list (car a) (max (cadr a) (cadr b))))


(define (myless a b)
  (< (car a) (car b)))

(define (solve)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin
          (displayln (merge (sort (append a b) myless)))
          (solve)))))

(solve)