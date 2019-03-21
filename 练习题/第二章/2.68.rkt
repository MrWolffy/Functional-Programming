#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (helper now-tree bit-list)
    (cond ((null? now-tree) #f)
          ((leaf? now-tree)
           (if (eq? (symbol-leaf now-tree) symbol)
               bit-list
               #f))
          (else (let ((left-symbol
                       (helper (left-branch now-tree)
                               (append bit-list (list 0))))
                      (right-symbol
                       (helper (right-branch now-tree)
                               (append bit-list (list 1)))))
            (cond ((and (not left-symbol) (not right-symbol))
                   #f)
                  ((not left-symbol) right-symbol)
                  ((not right-symbol) left-symbol))))))
  (let ((ans (helper tree '())))
    (if (not ans)
        (error "bad symbol -- ENCODE-SYMBOL" symbol)
        ans)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(A D A B B C A))

(encode sample-message sample-tree)
; '(0 1 1 0 0 1 0 1 0 1 1 1 0)