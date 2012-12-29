;; from sicp exercise 1.44

(load "../lib/repeated.scm")

(load "../lib/smooth.scm")

(define (n-fold-smooth f n dx)
  ((repeated (lambda (f) (smooth f dx)) n) f))
