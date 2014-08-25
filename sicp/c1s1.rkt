#lang racket/base

(require rackunit
         rackunit/text-ui)

(define ex-1.1-tests
  (test-suite
   "tests for equality"
   (check-equal? 10 10)
   (check-equal? (+ 5 4 9) 18)
   (check-equal? (- 9 1) 8)
   (check-equal? (/ 6 2) 3)
   (check-equal? (+ (* 2 4) (- 4 6)) 6)
   ;; (define ...) does not return anything on the repl
   (let* ((a 3)
          (b (+ a 1)))
     (check-equal? (+ a b (* a b)) 19)
     (check-equal? (= a b) #f)

     (check-equal?
      (if (and (> b a) (< b (* a b)))
          b
          a)
      4)
     (check-equal?
      (cond ((= a 4) 6)
            ((= b 4) (+ 6 7 a))
            (else 25))
      16)
     (check-equal?
      (+ 2 (if (> b a) b a))
      6)
     (check-equal?
      (* (cond ((> a b) a)
               ((< a b) b)
               (else -1))
         (+ a 1))
      16))))


(define ex-1.2-tests
  (test-suite
   "translate expression into prefix-notation"
   (check-equal?
    (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
       (* 3 (- 6 2) (- 2 7)))
    -37/150)))



(define (square x)
  (* x x))

(define (sum-of-squares-of-larger-two a b c)
  (let ((a2 (square a))
        (b2 (square b))
        (c2 (square c)))
    (+ a2 b2 c2 (- (min a2 b2 c2)))))


(define ex-1.3-tests
  (test-suite
   "Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers."

   (check-equal? (sum-of-squares-of-larger-two 1 2 3)
                 13)

   (check-equal? (sum-of-squares-of-larger-two 4 2 3)
                 25)

   (check-equal? (sum-of-squares-of-larger-two 5 6 3)
                 61)

   )
  )

#|
Exercise 1.4.  Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

  (define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))

Answer:

Since the operator (if...) either returns a + or a -,
if b < 0, the expression becomes (- a b)
if b > 0, the expression becomes (+ a b)

Either case is equivalent to "a plus the abs value of b"
|#


#|
Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order
evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

Then he evaluates the expression

(test 0 (p))

What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What
behavior will he observe with an interpreter that uses normal-order evaluation? Explain your
answer. (Assume that the evaluation rule for the special form if is the same whether the
interpreter is using normal or applicative order: The predicate expression is
evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)


Answer:

In the applicative case, `test` will evaluate both 0 and (p). However, (p) will loop infinitely.
In the normal order case, the evaluation will follow:

(test 0 (p)) ; 0 -> 0, (p) -> (p), test -> (if...)
(if (= 0 0)
    0
    (p)) ; 0 -> 0
0

I *think* this is the case. I might actually be a little wrong, because it seems as though there should be a discussion
of environments here that I am missing. There is no discussion on environments in this section, so I assume it is
over-thinking, but it *seems* that in a normal-order situation the evaluation of (p) would be infinite,
because the p inside would need to be expanded. I guess this is why y combinator.
|#


(run-tests ex-1.1-tests)
(run-tests ex-1.2-tests)
(run-tests ex-1.3-tests)
