;; sicp exercise 2.1
;; make-rat will be move to lib/make-rat.scm

(define (make-rat n d)
  (let ((g (gcd n d))
	(sign (if (or (and (< n 0) (> d 0)) (and (> n 0) (< d 0)))
		  -1
		  1)))
    (cons (* sign (abs (/ n g))) (abs(/ d g)))))

(make-rat 10 -5)