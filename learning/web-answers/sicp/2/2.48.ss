#lang scheme

(define (make-segment v w)
  (list v w))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))