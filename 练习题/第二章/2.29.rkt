#lang racket

; 这个题简直太烦了

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; (a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; (b)
(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile)) 
       (branch-weight (right-branch mobile))))

(define (branch-weight branch)
    (if (hangs-another-mobile? branch)
        (total-weight (branch-structure branch))
        (branch-structure branch)))

(define (hangs-another-mobile? branch)
    (pair? (branch-structure branch)))

; (c)
(define (branch-torque branch)
    (* (branch-length branch)
       (branch-weight branch)))

(define (mobile-balance? mobile)
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
        (and
            (same-torque? left right)
            (branch-balance? left)
            (branch-balance? right))))

(define (same-torque? left right)
    (= (branch-torque left)
       (branch-torque right)))

(define (branch-balance? branch)
    (if (hangs-another-mobile? branch)
        (mobile-balance? (branch-structure branch))
        #t))

; (d)
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

