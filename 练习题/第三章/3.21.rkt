#lang racket
(require r5rs)

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

; 打印queue的时候首先打印queue的car
; 这一car指向整个队列，所以打印的第一项应该代表整个队列
; 之后打印queue的cdr
; 这一cdr指向表的末尾一项
; 如果插入了又删除
; 根据delete-queue的写法，删除时是不更改表末尾的
; 所以queue的cdr会一直指向最近被插入的一项

(define (print-queue queue)
  (displayln (car queue)))
