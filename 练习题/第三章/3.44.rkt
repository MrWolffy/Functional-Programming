#lang racket

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

; 如果withdraw的时候余额不足，那么程序会崩溃