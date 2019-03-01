#lang racket

(define (inner-loop rbeg wbeg rend wend)
  (if (eq? wbeg wend)
      (newline)
      (begin (display (+ wbeg 1))
             (display ",")
             (inner-loop rbeg (+ wbeg 1) rend wend))))

(define (loop rbeg rend wend)
  (if (eq? rbeg rend)
      (void)
      (begin (inner-loop rbeg 0 rend wend)
             (loop (+ rbeg 1) rend wend))))

(define (solve)
  (let ((r (read))
        (w (read)))
    (if (eq? r eof)
        (void)
        (begin (loop 0 r w)
               (solve)))))

(solve)

  