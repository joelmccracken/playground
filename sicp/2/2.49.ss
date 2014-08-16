#lang scheme
(require (only-in "../lib.ss" paint segments->painter make-segment make-vect))

(define almost-one .99) ; stupid; dr scheme doesn't want to print this properly if it is at one.
(define painter-a (segments->painter (list (make-segment (make-vect 0 0)
                                                         (make-vect 0 almost-one))
                                           (make-segment (make-vect 0 almost-one)
                                                         (make-vect almost-one almost-one))
                                           (make-segment (make-vect almost-one almost-one)
                                                         (make-vect almost-one 0))
                                           (make-segment (make-vect almost-one 0)
                                                         (make-vect 0 0)))))
                                 
                                                       
(paint painter-a) 

(define painter-b (segments->painter (list (make-segment (make-vect 0 0)
                                                         (make-vect 1 1))
                                           (make-segment (make-vect 1 0)
                                                         (make-vect 0 1)))))
(paint painter-b)


(define painter-c (segments->painter (list (make-segment (make-vect .5 0)
                                                         (make-vect 0 .5))
                                           (make-segment (make-vect 0 .5)
                                                         (make-vect .5 1))
                                           (make-segment (make-vect .5 1)
                                                         (make-vect 1 .5))
                                           (make-segment (make-vect 1 .5)
                                                         (make-vect .5 0)))))



(paint painter-c)

(define (easy-painter-maker l)
  (segments->painter (map (lambda (x) (make-segment (make-vect (car (car x))
                                                               (cadr (car x)))
                                                    (make-vect (car (cadr x))
                                                               (cadr (cadr x)))))
                          l)))



(define wave (easy-painter-maker 
              '(((0 0) (1 0)) ; box-bottom
                ((0 0) (0 1)) ; box-left
                ((0 8/10) (2/10 6/10)) ; right hand top
                ((0 6/10) (2/10 4/10)) ; right hand bottom
                ; too lazy to do any more. 
                )))
(paint wave)