#lang racket
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? seqs)
      '()
      (cons (accumulate op init
(if (null? (car seqs)) '() (accumulate
                                 (lambda (x y) (cons (car x) y))
                                 '()
                                 seqs)))
            (let ((a (if (null? (car seqs)) init
            (accumulate-n op init  (accumulate
                                   (lambda (x y) (cons (cdr x) y))
                                   '()
                                   seqs)))))
              (if (equal? a (cons init init))
                  '()
                  a)))))
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(display (accumulate-n + 10 s)) (newline)
(display (accumulate-n * 1 s)) (newline)
(display (accumulate-n cons '() s)) (newline)
(display "******") (newline)
(define (myloop)
  (let ((lst (read)))
    (if (eq? lst eof)
        (void)
        (begin (display (accumulate-n + 0 lst)) (newline)
               (display (accumulate-n cons '(a) lst)) (newline)
               (myloop)))))

             

(myloop)