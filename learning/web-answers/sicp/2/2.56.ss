#lang scheme

(require (only-in "../lib.ss" =number? number? variable? same-variable? sum? make-sum addend augend product? make-product multiplier multiplicand))
                  
                  
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

        
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (second x))

(define (exponent x)
  (third x))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))