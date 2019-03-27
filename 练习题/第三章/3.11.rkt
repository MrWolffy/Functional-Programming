#lang racket

; make-account创建一个环境E1
; deposit和withdraw均创建在这个环境下，分别为E2和E3
; acc2重新在global下创建一个环境
; make-account的函数体被共享