#lang racket

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map (lambda (s) (stream-first s)) argstreams))
       (apply stream-map
              (cons proc (map (lambda (s) (stream-rest s)) argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (partial-sums s)
  (stream-cons (stream-first s)
               (add-streams (stream-rest s)
                            (partial-sums s))))