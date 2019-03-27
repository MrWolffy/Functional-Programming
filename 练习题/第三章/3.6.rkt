#lang racket

(define rand
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate)
             (lambda ()
               (set! x (rand-update x))
               x))
            ((eq? op 'reset)
             (lambda (new-value)
               (set! x new-value)))))))
