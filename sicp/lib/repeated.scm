;; created for sicp exercise 1.43

(load "../lib/compose.scm")

(define (repeated f n)
  (cond ((= 1 n) (lambda (x) (f x)))
	(else (compose f (repeated f (- n 1))))))
