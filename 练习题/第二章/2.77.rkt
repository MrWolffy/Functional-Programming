#lang racket

; 因为2-24里的复数z是一个高级复数，有两层标签
; get首先从表中查找外层标签complex对应的magnitude
; 但表中只有rectangular和polar对应的magnitude
; 查找不到就会报错

; 高级复数的magnitude调用apply-generic过程
; apply-generic首先查找到complex对应的magnitude
; 然后执行低级复数的magnitude
; 而低级复数定义的magnitude又会调用apply-generic过程
; apply-generic再次查找到rectangular/polar对应的magnitude
; 然后执行rectangular/polar对应的magnitude