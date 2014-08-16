;; from sicp exercise 1.44

(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

