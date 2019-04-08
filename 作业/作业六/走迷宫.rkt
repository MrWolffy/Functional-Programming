#lang racket
(require r5rs)

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

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


(define visited '())


(define (read-table r c table)
  (loop1 r c table 0))

(define (loop1 r c table nowr)
  (if (= r nowr)
      table
      (begin (loop2 r c table nowr 0)
             (loop1 r c table (+ nowr 1)))))

(define (loop2 r c table nowr nowc)
  (if (= c nowc)
      table
      (let ((maze (read)))
        (insert! nowr nowc maze table)
        (loop2 r c table nowr (+ nowc 1)))))

(define (search table qu r c)
  (if (empty-queue? qu)
      (displayln "inf")
      (let ((front (front-queue qu)))
        (let ((nowr (car front))
              (nowc (cadr front))
              (nowk (caddr front))
              (step (cadddr front)))
          (set! visited (cons (list nowr nowc) visited))
          (delete-queue! qu)
          (if (and (= nowr r) (= nowc c))
              (displayln step)
              (begin (try-insert qu table (- nowr 1) nowc nowk (+ step 1) r c)
                     (try-insert qu table (+ nowr 1) nowc nowk (+ step 1) r c)
                     (try-insert qu table nowr (- nowc 1) nowk (+ step 1) r c)
                     (try-insert qu table nowr (+ nowc 1) nowk (+ step 1) r c)
                     (search table qu r c)))))))

(define (try-insert qu table nowr nowc nowk step r c)
  (if (or (< nowr 0) (< nowc 0) (> nowr r) (> nowc c))
      (void)
      (let ((maze (lookup nowr nowc table)))
        (if (member (list nowr nowc) visited)
            (void)
            (cond ((eq? maze 'W) (void))
                  ((eq? maze 'M)
                   (if (< (- nowk 1) 0)
                       (void)
                       (insert-queue! qu (list nowr nowc (- nowk 1) step))))
                  (else (insert-queue! qu (list nowr nowc nowk step))))))))

(define (myloop n)
  (if (= n 0)
      (void)
      (begin
        (set! visited '())
        (let ((r (read)) (c (read)) (k (read)))
          (let ((table (read-table r c (make-table)))
                (qu (insert-queue! (make-queue) (list 0 0 k 0))))
            (search table qu (- r 1) (- c 1))))
        (myloop (- n 1)))))

(myloop (read))