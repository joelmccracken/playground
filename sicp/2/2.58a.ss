#lang scheme

(require (only-in "../lib.ss" =number? number? variable? same-variable?
                   ))
                  
                  
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
  (and (pair? x) (eq? (cadr x) '**)))

(define (base x)
  (first x))

(define (exponent x)
  (third x))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list base '** exp))))

(define (make-sum a1 a2)

  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))

(define (augend s) 
  (caddr s))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (caddr p))


(deriv '(x + (3 * (x + (y + 2)))) 'x)