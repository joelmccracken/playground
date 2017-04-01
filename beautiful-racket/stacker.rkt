#lang br/quicklang

(require srfi/1)

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))

(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     (display
      (fold handle
              empty
              (list HANDLE-EXPR ...)))))

(provide (rename-out [stacker-module-begin #%module-begin]))

(define (handle [arg #f] [stack empty])
  (define result (cond
    [(number? arg)
     (cons arg stack)]
    [(or (equal? + arg) (equal? * arg))
     (define op-result (arg (first stack) (first [rest stack])))
     (cons op-result (rest (rest stack)))
     ]
    [#t stack]
  ))
  result
  )

(provide handle)

(provide * +)