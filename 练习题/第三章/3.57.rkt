#lang racket

; n-2次就可以
; 在计算fib(n)时需先计算fib(n-1)和fib(n-2)
; 计算fib(n-1)时又需先计算fib(n-2)和fib(n-3)
; 因此计算每一个fib时都要分出两个分支
; 所以计算量是O(2^n)的