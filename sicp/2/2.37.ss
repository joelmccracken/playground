#lang scheme

(require (only-in "../lib.ss" dot-product accumulate-n))

(define (matrix-*-vector m v)
  (map (lambda (m) (dot-product m v)) m))

(define (transpose n)
  (accumulate-n cons '() n))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))


(define m '( (1 2 3) (4 5 6) (7 8 9) ))
(define v '( 10 11 12))

(define x '((1 2 3) (4 5 6)))
(define y '((7 8) (9 10) (11 12)))

(matrix-*-vector m v)
(matrix-*-matrix x y)