;; sicp exercise 2.6

(load "../lib/inc.scm")

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x)))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; (define (+ n m)
;; yeah, redefining + kinda sucks. gave me a rather 
;; unintelligable error.
(define (lambda-add n m)
  (if (= 0 m)
      n
      (lambda-add (lambda (f) (lambda (x) (f ((n f) x)))) (- m 1))))

(((lambda-add zero 4) square) 10)

(((lambda-add two 2) inc) 10)
