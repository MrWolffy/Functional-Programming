#lang racket

(define (make-monitored f)
  (let ((count 0))
    (lambda (op)
      (cond ((eq? op 'how-many-calls?) count)
            ((eq? op 'reset-count)
             (set! count 0))
            (else (begin (set! count (+ count 1))
                         (f op)))))))