#lang racket

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op init seqs)
  (if (null? seqs)
      init
      (op (car seqs)
          (accumulate op init (cdr seqs)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty_board (list '()))

(define (queens board-size)
   (define (queen-cols k)
     (if (= k 0)
         (list empty-board)
         (filter
          (lambda (positions) (safe? k positions))
          (flatmap
           (lambda (new-row)
             (map (lambda (rest-of-queens)
                    (adjoin-position new-row k rest-of-queens))
                  (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
    (queen-cols board-size))

  (define (make-position row col)
    (cons row col))
  
  (define (position-row position)
    (car position))
  
 (define (position-col position)
   (cdr position))
 
 (define empty-board null)
 
 (define (adjoin-position row col positions)
   (append positions (list (make-position row col))))
 
 (define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
     (other-queens (filter (lambda (q)
                             (not (= col (position-col q))))
                           positions)))
     (define (attacks? q1 q2)
       (or (= (position-row q1) (position-row q2))
           (= (abs (- (position-row q1) (position-row q2)))
              (abs (- (position-col q1) (position-col q2))))))
     (define (iter q board)
       (or (null? board)
           (and (not (attacks? q (car board)))
                (iter q (cdr board)))))
     (iter kth-queen other-queens)))
 
 (queens 8)

; 这样写的话每对一个new-row都要重复求一遍(queen-cols (- k 1))
; T * N * N
