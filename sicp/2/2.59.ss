#lang scheme
(require (only-in "../lib.ss" adjoin-set))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

(union-set '( 1 2 3) '(2 4 5 6))