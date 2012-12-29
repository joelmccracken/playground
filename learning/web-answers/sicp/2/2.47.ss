#lang scheme

; this file isnt really going to be runnable, but its so simple, it isnt significant

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))


(define (make-frame1 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame1 f)
  (car f))

(define (edge1-frame1 f)
  (cdr f))

(define (edge2-frame1 f)
  (cddr f))