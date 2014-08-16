#lang scheme

(require (only-in "../lib.ss" accumulate nil))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y) ) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(append '(1 2 3) '(9 4 5))
(map (lambda (x) (+ 1 x)) '(1 2 3))
(length '(a b c))