#lang racket

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map (lambda (s) (stream-first s)) argstreams))
       (apply stream-map
              (cons proc (map (lambda (s) (stream-rest s)) argstreams))))))

(define s (stream-cons 1 (add-streams s s)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; 2^n，n从0开始