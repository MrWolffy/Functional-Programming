#lang racket
(require r5rs)

; 大致如此
; 内部再定义递归寻址过程即可

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup . keys)
      (define (lookup-helper keylst table)
        (let ((key-1 (car keylst)))
          (let ((record (assoc key-1 table)))
            (if record
                (if (null? (cdr keys))
                    (cdr record)
                    (let ((record1
                           (lookup-helper
                            (cdr keys)
                            (cdr record))))
                      (if record1 (cdr record1) false)))
                false))))
      (lookup-helper keys (cdr local-table)))
    (define (insert! value . keys)
      (define (insert-helper value keylst table)
        (let ((key-1 (car keylst)))
          (let ((subtable (assoc key-1 table)))
            (if (null? (cdr keys))
                (if subtable
                  (set-cdr! subtable value)
                  (set-cdr! subtable
                            (cons (cons key-1 value)
                                  (cdr subtable))))
                (let ((record2
                       (insert-helper value (cdr subtable) (cdr subtable))))
                  (set-cdr! local-table
                            (cons (list key-1 (cons key-2 value))
                                  (cdr local-table)))))))
            'ok)
      (insert-helper value keys (cdr local-table)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

