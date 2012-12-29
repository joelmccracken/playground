#lang scheme

(require "up-split.ss"
         "right-split.ss"
         (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

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

(provide corner-split)