#lang scheme

(define (same-parity . list)
  (define (filter-list list filter)
    (cond ((null? list) list)
          ((filter (car list))
           (cons (car list) (filter-list (cdr list) filter)))
          (else (filter-list (cdr list) filter))))
  (if (even? (car list))
      (filter-list list even?)
      (filter-list list odd?)))

    