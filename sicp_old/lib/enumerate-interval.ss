#lang scheme

(require "nil.ss")

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(provide enumerate-interval)