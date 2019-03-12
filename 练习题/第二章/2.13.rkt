#lang racket

; a = (a * (1 - pa%), a * (1 + pa%))
; b = (b * (1 - pb%), b * (1 + pb%))
; a * b
; = (a * b * (1 - pa%) * (1 - pb%), 
;    a * b * (1 + pa%) * (1 + pb%))
; ~ (a * b * (1 - (pa% + pb%)), 
;    a * b * (1 + (pa% + pb%)))
; percent(a * b) = percent(a) + percent(b)