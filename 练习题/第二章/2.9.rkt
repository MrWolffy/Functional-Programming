#lang racket

; 对于区间加：
; a + b = (low(a) + low(b), up(a) + up(b))
; length(a + b)
; = (up(a) + up(b)) - (low(a) + low(b))
; = (up(a) - low(a)) + (up(b) + low(b))
; = length(a) + length(b)
;
; 对于区间减：
; a - b = (low(a) - up(b), up(a) - low(b))
; length(a - b)
; = (up(a) - low(b)) - (low(a) - up(b))
; = (up(a) - low(a)) + (up(b) - low(b))
; = length(a) + length(b)
;
; 由于正负，乘除的区间宽度无法确定