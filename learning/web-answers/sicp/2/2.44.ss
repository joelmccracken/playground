#lang scheme

(require (only-in "../lib.ss" paint number->painter diagonal-shading 
                  paint-hires below beside rotate90 rotate270 rotate180 einstein right-split))



; from the section
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


; up-split is copied to the lib directory s.t. it can be used for future problems.
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (corner-split einstein 2))
      