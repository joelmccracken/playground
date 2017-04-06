#lang br/quicklang
(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod bf/expander_fn
                          ,parse-tree))
  (datum->syntax #f module-datum))

(require brag/support)
(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer
       [(eof) eof]
       [(char-set "><-.,+[]") lexeme]
       [any-char (next-token)]))
    (bf-lexer port))
  next-token)

(provide read-syntax)
