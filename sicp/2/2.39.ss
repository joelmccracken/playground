#lang scheme

(require (only-in "../lib.ss" fold-right fold-left))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))


(define (reverse-left sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

(reverse-right '( 1 2 3 4 5))
(reverse-left '(1 2 3 4 5))
