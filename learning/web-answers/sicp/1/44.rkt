;; sicp exercise 1.44
;; smooth will be moved to lib/smooth.scm
;; n-fold-smooth will be moved to lib/n-fold=smooth.scm
(load "../lib/repeated.scm")

(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n dx)
  ((repeated (lambda (f) (smooth f dx)) n) f))

((n-fold-smooth square 5 .1) 2)