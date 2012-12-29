#lang scheme

(require (prefix-in std: srfi/1))

(define (sicp:fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(std:fold cons '() '(1 2 3 4 5))
(std:fold-right cons '() '(1 2 3 4 5))
(sicp:fold-left cons '() '(1 2 3 4 5))

