;; sicp exercise 1.41
;; double will be moved to lib/double.scm
(load "../lib/inc.scm")

(define (double g)
  (lambda (x) (g (g x))))

(((double (double double)) inc) 5)
;; this returns 21