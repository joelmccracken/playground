(proclaim '(optimize speed))

(defun double (x) (* x 2))

(double 10)

(setq double 2)

(double double)

(symbol-value 'double)

(setf (get 'dog 'behavior)
      #'(lambda ()
      		(wag-tail)
		(bark)))

(mapcar #'(lambda (x y) (+ 1 x))
	'(1 2 3 4 5)
	'(1 2 3 4 5))


(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
	     (if (consp lst)
		 (+ (if (eq (car lst) obj) 1 0 )
		    (instances-in (cdr lst)))
		 0)))
    (mapcar #'instances-in lsts)))

(count-instances 'a '((a b c) (d a r p a) (d a r) (a a )))

(compiled-function-p #'double)

(compile 'double)

sqr