#lang racket



(require rackunit)


(define (ptriangle-row prev-row)
  (define (ptriangle-row-1 prev-row)
    (cond ((null? prev-row) null) ;; this shouldn't happen except for at the pyramid top
          ((null? (cdr prev-row)) (list 1))
          (else (cons (+ (car prev-row) (cadr prev-row))
                      (ptriangle-row-1 (cdr prev-row)))))) 
  (cons 1 (ptriangle-row-1 prev-row)))







(check-equal? (ptriangle-row '()) '(1))
(check-equal? (ptriangle-row '(1)) '(1 1))
(check-equal? (ptriangle-row '(1 1)) '(1 2 1))
(check-equal? (ptriangle-row '(1 2 1)) '(1 3 3 1))
(check-equal? (ptriangle-row '(1 3 3 1)) '(1 4 6 4 1))




(define (ptriangle depth)  
  (define (ptriangle-1 current-depth prev-row) 
    (if (= current-depth depth)
        null
        (let ((this-row (ptriangle-row prev-row)))
          (cons this-row
                (ptriangle-1 (+ 1 current-depth) this-row)))))
  (ptriangle-1 0 '()))

(check-equal? (ptriangle     1) '((1)))

(check-equal? (ptriangle     2) '( (1) 
                                  (1 1)))

(check-equal? (ptriangle     3) '(  (1) 
                                   (1 1) 
                                  (1 2 1)))

(check-equal? (ptriangle     4) '(   (1) 
                                    (1 1) 
                                   (1 2 1) 
                                  (1 3 3 1)))

(check-equal? (ptriangle     5) '(    (1) 
                                     (1 1) 
                                    (1 2 1) 
                                   (1 3 3 1) 
                                  (1 4 6 4 1)))

(check-equal? (ptriangle 6) '(     (1) 
                                  (1 1) 
                                 (1 2 1) 
                                (1 3 3 1) 
                               (1 4 6 4 1) 
                              (1 5 10 10 5 1)))

