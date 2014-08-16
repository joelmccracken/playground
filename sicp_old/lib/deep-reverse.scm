#lang scheme
; made for exercise 2.27

(define (deep-reverse list)
  (define (iter list rev)
    (if (null? list)
        rev
        (iter (cdr list) (cons (deep-reverse (car list)) (rev)))))
  (iter list null))
