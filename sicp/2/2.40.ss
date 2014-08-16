#lang scheme

; todo: need to make a good prime? function
; need to get the rest of the example code into the library

(require (only-in "../lib.ss" flatmap nil make-pair-sum prime-sum? enumerate-interval))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; dont know if this will need to be in the lib eventually
(define (unique-pairs n)
  (flatmap (lambda (j) 
             (map (lambda (i) (list i j))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 2 10)))


(prime-sum-pairs 10)