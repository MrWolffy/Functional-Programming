#lang racket

; 这什么破题，不做了
; 在CSDN上抄的
; 不可能完全不超过两次的，有些答案是错的

(define (make-interval a b) (cons a b))

(define (lower-bound a) (car a))

(define (upper-bound a) (cdr a))

(define (mul-interval x y)
  (define (less? a)
    (if (or(< (upper-bound a) 0)(= (upper-bound a) 0))
        #t
        #f))
  (define (mid? a)
    (if (and(< (lower-bound a) 0)(> (upper-bound a) 0))
        #t
        #f))
  (define (more? a)
    (if (or(> (lower-bound a) 0)(= (lower-bound a) 0))
        #t
        #f))
  (cond ((and(less? x)(less? y))(make-interval (* (upper-bound x)(upper-bound y))
                                               (* (lower-bound x)(lower-bound y))))
        ((and(less? x)(mid? y))(make-interval (* (lower-bound x)(upper-bound y))
                                               (* (upper-bound x)(lower-bound y))))
        ((and(less? x)(more? y))(make-interval (* (lower-bound x)(upper-bound y))
                                               (* (upper-bound x)(lower-bound y))))
        ((and(mid? x)(less? y))(make-interval (* (upper-bound x)(lower-bound y))
                                               (* (lower-bound x)(lower-bound y))))
        ((and(mid? x)(mid? y))(let((p1(* (upper-bound x)(upper-bound y)))
                                                  (p2(* (upper-bound x)(lower-bound y)))
                                                  (p3(* (lower-bound x)(upper-bound y)))
                                                  (p4(* (lower-bound x)(lower-bound y))))
                                               (make-interval (min p1 p2 p3 p4)
                                                              (max p1 p2 p3 p4))))
        ((and(mid? x)(more? y))(make-interval (* (lower-bound x)(upper-bound y))
                                               (* (upper-bound x)(upper-bound y))))
        ((and(more? x)(less? y))(make-interval (* (upper-bound x)(lower-bound y))
                                               (* (lower-bound x)(upper-bound y))))
        ((and(more? x)(mid? y))(make-interval (* (upper-bound x)(lower-bound y))
                                               (* (upper-bound x)(upper-bound y))))
        ((and(more? x)(more? y))(make-interval (* (lower-bound x)(lower-bound y))
                                               (* (upper-bound x)(upper-bound y))))))

