#lang racket

(define (barber n)
  (define (helper op . t)
    (cond ((eq? op 'add) (barber (+ n (car t))))
          ((eq? op 'sub)
           (barber (if (< (- n (car t)) 0) 0 (- n (car t)))))
          ((eq? op 'query) n)))
  helper)

(define (make-barber n)
  (if (= n 0)
      '()
      (cons (barber 0) (make-barber (- n 1)))))

(define (serve barbers m)
  (if (= m 0)
      (void)
      (let ((p (read))
            (q (read)))
        (set! barbers
              (map (lambda (x) (x 'sub p)) barbers))
        (serve (find-chair barbers q 0) (- m 1)))))

(define (find-chair barbers q n)
  (if (null? barbers)
      (begin
        (displayln "Failed")
        '())
      (let ((this-barber (car barbers)))
        (if (= (this-barber 'query) 0)
            (begin (displayln n)
                   (cons (this-barber 'add q)
                         (cdr barbers)))
            (cons this-barber
                  (find-chair (cdr barbers) q (+ n 1)))))))

      
(define (myloop)
  (let ((n (read))
        (m (read)))
    (if (eq? n eof)
        (void)
        (begin (serve (make-barber n) m)
               (myloop)))))

(myloop)
                  
                  