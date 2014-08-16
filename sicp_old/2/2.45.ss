#lang scheme

(require (only-in "../lib.ss" paint number->painter diagonal-shading 
                  paint-hires below beside rotate90 rotate270 rotate180 einstein))


(define (split orig sm)
  (define (me painter n)
    (if (= n 0)
        painter
        (let ((smaller (me painter (- n 1))))
          (orig painter (sm smaller smaller)))))
  me)

(define right-split (split beside below))
(define up-split (split below beside))



(paint (right-split einstein 3))
(paint (up-split einstein 3))