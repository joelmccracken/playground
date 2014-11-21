#lang racket

(require rackunit
         rackunit/text-ui)


#|
exercise 1.9

1st:

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

2nd:

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

1st:
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc 5))))


2nd:
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)


1st is recursive. 2nd is iterative.

|#

;; exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

#|

f(n) = A(0,n) = 2n
g(n) = A(1, n) =
(A 0 (A 1 (- n 1))) =
(* 2 (A 1 (- n 1))) =
g(0) = 0
g(n) = 2^n

h(n) = A(2, n).

analysis:

(A 2 0) = 0.
(A 2 1) = 2.
(A 2 2) =
(A 1 (A 2 1)) =
(A 1 2) =
(A 0 (A 1 1)) =
(A 0 2) = 4.
(A 2 3) =
(A 1 (A 2 2)) =
(A 1 4) =
(A 0 (A 1 3)) =
(A 0 (expt 2 3)) =
(A 0 8) = 16.
(A 2 4) =


I gave up, here, and found the answer on google. It is 2 re-squared n times.
I've never seen this pattern, and I don't know how it could have been derived from
the problem. C'est la vie.

In hindsight, I think I could have come up with this if I had solved this
out for a few more cases.
|#

(define ex-1.10-tests
  (test-suite
   "ackermann function values"

   (check-equal? (A 1 10) 1024)
   (check-equal? (A 2 4) 65536)
   (check-equal? (A 3 3) 65536)))

(define ex-1.11-tests
  (test-suite
   "in-place vs tree-recursive function definitions"
   (let ()
     (define (f n)
       (cond ((< n 3) n)
             (true 6)))
     (f 1))))


(provide ex-1.10-tests)
(provide ex-1.11-tests)

;; (run-tests ex-1.10-tests)
;; (run-tests ex-1.11-tests)


