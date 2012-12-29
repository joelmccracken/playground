;; sicp exercise 1.43
;; repeated will be moved to lib/repeated.scm

(load "../lib/compose.scm")

(define (repeated f n)
  (cond ((= 1 n) (lambda (x) (f x)))
	(else (compose f (repeated f (- n 1))))))

((repeated square 2) 5)
