#lang scheme
(require (only-in "../lib.ss" accumulate-n))

; my answer to this question is now in lib/accumulate-n.ss
; and is part of my library

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
