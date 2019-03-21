#lang racket

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (tramform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below2 painter1 painter2)
  (rotate270 (beside (rotate90 painter1)
                     (rotate90 painter2))))
  