;; sicp exercise 2.8
;; sub-interval wiill be moved to lib/interval.scm


(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))


