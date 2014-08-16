;; sicp exercise 2.9

(load "../lib/interval.scm")


(define (interval-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; these are equal

(+ (interval-width (make-interval 1.5 2.4))
   (interval-width (make-interval 5.6 9.8)))

(interval-width (add-interval (make-interval 1.5 2.4)
			      (make-interval 5.6 9.8)))

(- (interval-width (make-interval 1.5 2.4))
   (interval-width (make-interval 5.6 9.8)))

(interval-width (sub-interval (make-interval 1.5 2.4)
			      (make-interval 5.6 9.8)))


;; these are not

(* (interval-width (make-interval 1.5 2.4))
   (interval-width (make-interval 5.6 9.8)))

(interval-width (mul-interval (make-interval 1.5 2.4)
			      (make-interval 5.6 9.8)))

(/ (interval-width (make-interval 1.5 2.4))
   (interval-width (make-interval 5.6 9.8)))

(interval-width (div-interval (make-interval 1.5 2.4)
			      (make-interval 5.6 9.8)))

