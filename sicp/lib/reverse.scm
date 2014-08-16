
(define (reverse list)
  (define (reverse-iter list rev)
    (if (null? list)
        rev
        (reverse-iter (cdr list) (cons (car list) rev))))
  (reverse-iter list null))