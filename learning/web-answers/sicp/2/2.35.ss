#lang scheme

(require (only-in "../lib.ss" accumulate))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (list? x)
                             (count-leaves x)
                             1)) 
                       t)))

(count-leaves '(1 (2 3) 8))

