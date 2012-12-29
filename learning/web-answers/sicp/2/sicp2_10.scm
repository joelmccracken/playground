;; sicp exercise 2.10
;; changed div-interval moved to lib/interval.scm

(define (div-interval x y)
  (if (= (interval-width y) 0)
      0
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

