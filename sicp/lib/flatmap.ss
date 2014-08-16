#lang scheme

(require "accumulate.ss")
(define nil '())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(provide flatmap)