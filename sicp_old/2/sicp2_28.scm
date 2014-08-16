;(load "../lib/reverse.scm")

(define (fringe list)
  (define (fringe-iter list the-fringe)
    (cond ((null? list) the-fringe)
          ((list? (car list))  (fringe-iter (cdr list) (fringe-iter (car list) the-fringe)))
          (else (fringe-iter (cdr list) (cons (car list) the-fringe)))))
  (reverse (fringe-iter list '())))

(define x (list (list 1 2) (list 3 4)))


(fringe x)
;(1 2 3 4)

(fringe (list x x))
;(1 2 3 4 1 2 3 4)