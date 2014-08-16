(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))



(define (total-weight mobile)
  (if (not (list? mobile)) 
      mobile
      (+ (total-weight (branch-structure (right-branch mobile)))
         (total-weight (branch-structure (left-branch mobile))))))


(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (cond ((not (list? mobile)) #t)
        ((and (= (torque (left-branch mobile))
                 (torque (right-branch mobile)))
              (balanced? (branch-structure (left-branch mobile)))
              (balanced? (branch-structure (right-branch mobile))))
         #t)
        (else #f)))

(define mob (make-mobile (make-branch 3 
                                      (make-mobile (make-branch 1 2)
                                                   (make-branch 2 3)))
                         (make-branch 4
                                      (make-mobile (make-branch 13 6)
                                                   (make-branch 7 8)))))

(define mob2 (make-mobile (make-branch 3
                                       4)
                          (make-branch 4
                                       3)))

(define mob3 (make-mobile (make-branch 3
                                       (make-mobile (make-branch 9 8)
                                                    (make-branch 8 9)))
                          (make-branch 3
                                       (make-mobile (make-branch 8 9)
                                                    (make-branch 9 8)))))

(total-weight mob)
(balanced? mob)
(balanced? mob2)
(balanced? mob3)