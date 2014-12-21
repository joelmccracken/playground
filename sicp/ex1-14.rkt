#lang racket


(define (gen-count-change-graph)
  
  (define output-filename "ex1-14.dot")
  (when (file-exists? output-filename) 
      (delete-file output-filename))
  (define output (open-output-file output-filename))
  (define counter 0)
  (define (++counter)
    (let ([ncounter (add1 counter)])
      (set! counter ncounter)
      ncounter))
  
  (define (record-cc amount kinds-of-coins my-id)
    (fprintf output 
             "\texec~s [label=\"<f0>cc|<f1>~s|<f2>~s\"];\n"
             my-id amount kinds-of-coins))
 
  (define (record-1 id parent-id)
    (fprintf output "\toneval~s [label=\"1\"];\n" id)
    (fprintf output "\texec~s -> oneval~s;\n" parent-id id))
 
  
  (define (record-0 id parent-id)
    (fprintf output "\tzedval~s [label=\"0\"];\n" id)
    (fprintf output "\texec~s -> zedval~s;\n" parent-id id))
  
  (define (record-branching left-child-id right-child-id parent-id)
    (fprintf output 
             "\texec~s -> {exec~s; exec~s}\n"
             parent-id
             left-child-id
             right-child-id
             )
    )
  
  
  (define (count-change amount)
    (cc amount 5 counter))
  
  (define (cc amount kinds-of-coins my-id)
    (record-cc amount kinds-of-coins my-id)
    (let ([left-child-id (++counter)]
          [right-child-id (++counter)])
      (cond ((= amount 0) 
             (begin
               (record-1 left-child-id my-id)
               1
               ))
            ((or (< amount 0) (= kinds-of-coins 0))
             (begin
               (record-0 left-child-id my-id)
               0))
             (else (begin 
                     (record-branching left-child-id right-child-id my-id)
                     (+ (cc amount 
                          (- kinds-of-coins 1)
                          left-child-id
                          )
                      (cc (- amount 
                             (first-denomination kinds-of-coins))
                          kinds-of-coins
                          right-child-id
                          )))))))

  
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  
  (display "digraph execution_tree {
node [shape=record];
" output)
  
  (count-change 11)
  (display " } " output)
  (close-output-port output))
  

(gen-count-change-graph)