#lang scheme

(require "lib.scm"
         srfi/1)

(define accumulate fold-right)
(define (enumerate-interval start stop)
  (iota stop start))
(define nil '())

(define (square x) (* x x))


;; this prime stuff is not at all the best
;; test for primality, but whatever. 
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(provide accumulate
         enumerate-interval
         nil
         square
         prime?)