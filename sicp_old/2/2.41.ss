#lang scheme

(require (only-in "../lib.ss" enumerate-interval flatmap))

(define (make-triples n)
  (flatmap (lambda (i)
         (flatmap (lambda (j)
                (map (lambda (k) (list i j k))
                     (enumerate-interval 1 n )))
              (enumerate-interval 1 n)))
       (enumerate-interval 1 n)))

(define (trips-lt-n-sum-to-s n s)
  (define (sum-to-s? l)
    (= s (+ (car l) (cadr l) (caddr l))))
  (filter sum-to-s? (make-triples n)))
 

;(make-triples 10)
(trips-lt-n-sum-to-s 10 20)