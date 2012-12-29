;; sicp exercise 1.46 
;; iterative-improve will be copied to lib/iterative-improve.scm

(load "../lib/average.scm")

(define (iterative-improve good-enough? improve-func)
  (lambda (guess) (if (good-enough? guess)
		      guess
		      ((iterative-improve good-enough? improve-func) (improve-func guess)))))

(define (sqrt x)
  (define (make-improve x)
    (lambda (guess) (average guess (/ x guess))))
  (define (make-good-enough? x)
    (lambda (guess) (< (abs (- (square guess) x)) .001)))
  ((iterative-improve (make-good-enough? x) (make-improve x)) 1.0))


(define (fixed-point f first-guess)
  (define tolerance 0.000000000000001)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))



(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)
;; answer should be something around 1.2587315962971173
(sqrt 16)
