#lang scheme
(require (only-in "../lib.ss" nil))


(define (equal? l1 l2)
  (cond ((or (eq? nil l1) 
             (eq? nil l2)) (and (eq? nil l1)
                                (eq? nil l2))); both should be nil at same time, this is the #t condition
        ((eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))
        (else #f)))

(equal? '(1 2 3) '(1 2 3))
(equal? '(1) '(d))
(equal? '(1 2 3) '(1 2 3 4))
(equal? '(1 2 3 4) '(1 2 3))
(equal? '(a b c) '(a b c))