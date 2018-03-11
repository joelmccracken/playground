#lang racket


(require lang-file/read-lang-file)
(read-lang-module (open-input-file "poem.html.pp"))



(require plot)

(plot (function sin (- pi) pi #:label "y = sin(x)"))