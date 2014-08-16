#lang scheme

(require "prime.ss")

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


(provide prime-sum?)