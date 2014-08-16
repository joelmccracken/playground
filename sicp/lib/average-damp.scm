; from section 1.3.4

(load "../lib/average.scm")

(define (average-damp f)
  (lambda (x) (average x (f x))))