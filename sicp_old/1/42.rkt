;; sicp exercise 1.42
;; compose will be saved to lib/compose.scm

(load "../lib/inc.scm")

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)
