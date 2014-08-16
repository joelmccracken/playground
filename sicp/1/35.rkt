;; sicp exercise 1.35
(load "../lib/fixed-point.scm")

(define (calc-phi y)
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0))

(calc-phi 10)
