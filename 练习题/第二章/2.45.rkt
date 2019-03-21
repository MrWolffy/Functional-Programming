#lang racket

(define (split first-split sub-split)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first-split sub-split)
                        painter (- n 1))))
          (first-split painter
                       (sub-painter smaller smaller))))))

