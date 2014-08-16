#lang scheme

(require (only-in "../lib.ss" paint einstein transform-painter make-vect rotate90 rotate270))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           ((transform-painter (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)) painter1))
          (paint-right
           ((transform-painter split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0)) painter2)))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (below-a painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           ((transform-painter split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0)) painter2))
          (paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point) painter1)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(paint (below-a einstein einstein))

(define (below-b painter1 painter2)
  (rotate270 (beside (rotate90 painter1) 
                     (rotate90 painter2))))

(paint (below-b einstein einstein))

