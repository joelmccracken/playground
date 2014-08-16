#lang scheme

(require (only-in "../lib.ss" =number? number? variable? same-variable?))
                  
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

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
  (if (pair? x)
      (let ((l (fix-products-then-sums x)))
        (eq? (cadr l) '+))
      #f))

(define (product? x)
  (if (pair? x)
      (let ((l (fix-products-then-sums x)))
        (eq? (cadr l) '*))
      #f))


; for every "top-level" * we encounter, take the thing before it and the thing after it, and embed them in parens
; begin: holds everything we've seen to this point
; rest: holds everything we have yet to look at
; for every look at rest, if the second item in rest is *, create a new rest s.t. the first item is a list of the first three items in old 
;      rest; then recurse. (remove all of these first three items from old rest)

(define (fix-products-then-sums x)
  (fix-sums (fix-products x)))
  
(define (fix-sums x)
  (define (r begin rest)
    (cond ((eq? rest '()) begin)
          ((= 1 (length rest))
           (append begin rest))
          ((eq? (second rest) '+)
           (r begin
              (append (list (list (first rest)
                                  (second rest)
                                  (third rest)))
                      (cdddr rest))))
          (else (r (append begin (list (car rest)))
                   (cdr rest)))))
  (make-list-outer-level (r '() x)))

(define (fix-products x)
  (define (r begin rest)
    (cond ((eq? rest '()) begin)
          ((= 1 (length rest))
           (append begin rest))
          ((eq? (second rest) '*)
           (r begin
              (append (list (list (first rest)
                                  (second rest)
                                  (third rest)))
                      (cdddr rest))))
          (else (r (append begin (list (car rest)))
                   (cdr rest)))))
  (make-list-outer-level (r '() x)))

(define (make-list-outer-level x)
  (if (= (length x) 1)
      (car x)
      x))

(define (addend s) (car (fix-products-then-sums s)))

(define (augend s) (caddr (fix-products-then-sums s)))

(define (multiplier p) (car (fix-products-then-sums p)))

(define (multiplicand p) (caddr (fix-products-then-sums p)))


;(deriv '(x + (3 * (x + (y + 2)))) 'x)
;(deriv '(x + 3 * (x + y + 2)) 'x)
;(deriv '(x + x + x + x) 'x)
(deriv '(x * x) 'x)
(deriv '(x * x * x) 'x)