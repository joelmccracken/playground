#lang scheme
; will be added to lib/reverse.scm


(define (reverse list)
  (define (reverse-iter list rev)
    (if (null? list)
        rev
        (reverse-iter (cdr list) (cons (car list) rev))))
  (reverse-iter list null))

(reverse (list 1 4 9 16 25))