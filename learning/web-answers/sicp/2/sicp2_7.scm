;; solution for sicp exercise 2.7
;; make-interval will be moved to lib/iterval.scm, along with all the supporting material


(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))


