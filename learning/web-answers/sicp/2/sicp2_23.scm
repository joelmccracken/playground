#lang scheme 

(define (for-each func list)
  (map func list)
  true)

(for-each (lambda (x) (newline) (display x))
          (list 57 89 009))