;; originally created in exercise 1.41
(define (double g)
  (lambda (x) (g (g x))))

