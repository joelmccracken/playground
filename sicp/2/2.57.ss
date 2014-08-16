#lang scheme
(require (only-in "../lib.ss" =number? number? variable? same-variable? sum? make-sum product? make-product
                  exponentiation? exponent make-exponentiation base))
                  
                  
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((exponentiation? exp)
         (make-product (exponent exp) 
                       (make-exponentiation (base exp)
                                            (- (exponent exp) 1))))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))



(define (addend s) (cadr s))

(define (augend s) 
  (if (= (length s) 3)
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (if (= (length p) 3)
      (caddr p)
      (cons '* (cddr p))))

(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ x y (* x 3)) 'x)