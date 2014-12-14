#lang scribble/base

@(require scribble/eval
          scribble/lp
          rackunit)

@title{SICP Exercise 1.11}

@nested{
  A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3.
  Write a procedure that computes f by means of a recursive process.
  Write a procedure that computes f by means of an iterative process.
}

@examples[
          (require rackunit)
          (define record-start #f)
          
          (define (get-continuation-size)
            (let ((results (make-vector 20)))
              (vector-set-performance-stats! results (current-thread))
              ;; item 3 contains continuation size
              (vector-ref results 3)))
  
          (define (record-stack name)
            (when record-start
              (displayln (list name (get-continuation-size)))))
       
               (define (f-recursive n)
            (record-stack "f-recursive")
            (if (< n 3)
                n
                (+ (f-recursive (- n 1))
                   (* 2 (f-recursive (- n 2)))
                   (* 3 (f-recursive (- n 3))))))

          (define (f-iterative n)
            (define (f-iter-intern at fn-1 fn-2 fn-3)              
              (record-stack "f-recursive")
              (define (calc-f-of-at)
                (+ fn-1
                   (* 2 fn-2)
                   (* 3 fn-3)))
              (if (= at n)
                  (calc-f-of-at)
                  (f-iter-intern (+ 1 at)
                                  (calc-f-of-at)
                                  fn-1
                                  fn-2)))
            (if (< n 3)
                n
                (f-iter-intern 3 2 1 0)))
          
          (define (test-f f)
            (check-eqv? (f 0) 0)
            (check-eqv? (f 1) 1)
            (check-eqv? (f 2) 2)
            (check-eqv? (f 3) 4)
            (check-eqv? (f 4) 11)
            (check-eqv? (f 5) 25))

          (test-f f-recursive)
          (test-f f-iterative)
          
          (set! record-start #t)
          (f-recursive 10)
          (f-iterative 10)
]

