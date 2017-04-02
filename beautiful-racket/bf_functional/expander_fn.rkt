#lang br/quicklang

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))

(provide (rename-out [bf-module-begin #%module-begin]))


(define-macro (bf-program OP-OR-LOOP-ARGS ...)
  #'(void
     (fold-fns (mk-state 0 (make-vector 100 0))
               (list OP-OR-LOOP-ARGS ...))))

(provide bf-program)

(define (fold-fns state cmds)
  (for/fold
   ([st state])
   ([fn cmds])
    (fn st)))

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(lambda (state)
      (for/fold ([st state])
                ([i (in-naturals)]
                 #:break (zero? (current-byte st)))
        (define cmds (list OP-OR-LOOP-ARG ...))
        (fold-fns st cmds))))

(provide bf-loop)

(define-macro-cases bf-op
  [(bf-op ">") #'gt]
  [(bf-op "<") #'lt]
  [(bf-op "+") #'plus]
  [(bf-op "-") #'minus]
  [(bf-op ".") #'period]
  [(bf-op ",") #'comma])
(provide bf-op)

(define (mk-state ptr mem) (list ptr mem))
(define (ptr state) (first state))
(define (mem state) (second state))
(define (current-byte state)
  (vector-ref (mem state)
              (ptr state)))

(define (update-ptr state op)
  (mk-state (op (ptr state))
            (mem state)))

(define (current-byte-updater op)
  (lambda (state)
    (define next-mem (vector-copy (mem state)))
    (vector-set! next-mem (ptr state) (op (current-byte state)))
    (mk-state (ptr state)
              next-mem)))

(define (ptr-updater op)
  (lambda (state)
    (update-ptr state op)))

(define plus (current-byte-updater add1))
(define minus (current-byte-updater sub1))

(define gt (ptr-updater add1))
(define lt (ptr-updater sub1))

(define comma (current-byte-updater read-byte))
(define (period state)
  (write-byte (current-byte state))
  state)
