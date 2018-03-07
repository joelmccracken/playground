#lang racket


(require lang-file/read-lang-file)
(read-lang-module (open-input-file "poem.html.pp"))