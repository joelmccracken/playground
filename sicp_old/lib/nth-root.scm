;; from sicp exercise 1.45 

(load "../lib/fixed-point.scm")
(load "../lib/average-damp.scm")
(load "../lib/repeated.scm")

(define (nth-root x n)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y (- n 1)))))
	       2.0))

