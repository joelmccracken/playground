(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (iter z a)
    (if (not (= 0 (remainder z 2)))
	a
	(iter (/ z 2) (+ a 1))))
  (iter z 0))

(define (cdr z)
  (define (iter z b)
    (if (not (= 0 (remainder z 3)))
	b
	(iter (/ z 3) (+ b 1))))
  (iter z 0))
       
