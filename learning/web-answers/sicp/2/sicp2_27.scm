#lang scheme
; made for sicp exercise 2.27

(define (deep-reverse list)
  (define (reverse-iter list rev)
    (cond ((null? list) rev)
          ((not (pair? list)) list)
          (else (reverse-iter (cdr list) 
                      (cons (deep-reverse (car list)) 
                            rev)))))
  (reverse-iter list null))


;(load "../lib/deep-reverse.scm")

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)
