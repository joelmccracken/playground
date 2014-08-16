;; created for sicp exercise 1.46 

(define (iterative-improve good-enough? improve-func)
  (lambda (guess) (if (good-enough? guess)
		      guess
		      ((iterative-improve good-enough? improve-func) (improve-func guess)))))
