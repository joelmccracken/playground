#lang scheme

(require "accumulate.ss")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(provide dot-product)