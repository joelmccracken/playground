#lang scheme

(define (square-list items)
  (if (null? items)
      '() ; dr scheme doesn't seem to like it some nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))
