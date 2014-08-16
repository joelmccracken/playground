;; sicp exercise 2.11

(define (mul-interval x y)
  (if (< (lower-bound x) (upper-bound x))
      (if (< (lower-bound y) (upper-bound y))
	  (make-interval (* (lower-bound x) (lower-bound y))
			 (* (upper-bound x) (upper-bound y)))
	  (make-interval (* (lower-bound x) (upper-bound y))
			 (* (upper-bound x) (lower-bound y))))
      (if (< (lower-bound y) (upper-bound y))
	  (make-interval (* (upper-bound x) (lower-bound y))
			 (* (lower-bound x) (upper-bound y)))
	  (make-interval (* (upper-bound x) (upper-bound y))
			 (* (lower-bound x) (lower-bound y))))))


;; haven't really tested this yet... not really interested in it, either