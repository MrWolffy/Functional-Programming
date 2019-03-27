#lang racket

(define (make-account balance init-pw)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw init-pw)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        (lambda (x) (displayln "Incorrect password"))))
  dispatch)

(define (make-joint account old-pw new-pw)
  (lambda (pw m)
    (if (eq? pw new-pw)
        (account old-pw m)
        (lambda (x) (displayln "Incorrect password")))))



(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 10)

