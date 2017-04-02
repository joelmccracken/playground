#lang br/quicklang

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))

(provide (rename-out [bf-module-begin #%module-begin]))


(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(void OP-OR-LOOP-ARG ...)
  )


(provide bf-program)

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(until (zero? (current-byte))
           OP-OR-LOOP-ARG ...))

(provide bf-loop)

(define-macro-cases bf-op 
  [(bf-op ">") #'(gt)]
  [(bf-op "<") #'(lt)]
  [(bf-op "+") #'(plus)]
  [(bf-op "-") #'(minus)]
  [(bf-op ".") #'(period)]
  [(bf-op ",") #'(comma)])

(provide bf-op)

(define arr (make-vector 30000 0))
(define ptr 0)

(define (current-byte)
  (vector-ref arr ptr))
;
(define (gt)
  (set! ptr (+ ptr 1)))
;
(define (lt)
  (set! ptr (- ptr 1)))
;
(define (plus)
  (vector-set! arr ptr (+ (vector-ref arr ptr) 1)))
;
(define (minus)
  (vector-set! arr ptr (- (vector-ref arr ptr) 1)))
;
(define (period)
  (write-char (integer->char (vector-ref arr ptr))))

(define (comma)
  (vector-set! arr ptr (char->integer (read-char))))