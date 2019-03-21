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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge
       (cons (make-code-tree (cadr pairs) (car pairs))
             (cddr pairs)))))

(define sample-pairs '((a 2) (na 16) (boom 1) (Sha 3)
                       (Get 2) (yip 9) (job 2) (Wah 1)))
(define sample-tree (generate-huffman-tree sample-pairs))
(define sample-message '(Get a job
                         Sha na na na na na na na na
                         Get a job
                         Sha na na na na na na na na
                         Wah yip yip yip yip yip yip yip yip yip
                         Sha boom))

(displayln (encode sample-message sample-tree))
; (1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0
;  1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0
;  1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
;  1 0 1 1 1 1 1 1 0)
(displayln (length (encode sample-message sample-tree)))
; 87

; 定长编码每个符号需要3个二进制位
; 一共36个单词，需要108个二进制位