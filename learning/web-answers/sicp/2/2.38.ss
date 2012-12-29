#lang scheme

(require (only-in "../lib.ss" fold-right fold-left nil))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

;; the property is the communicative property
;; consider the list x0 and x1
;; fold-left and fold-right would differ by:
;; (op x0 (op x1 first)) 
;; (op x1 (op x0 first))
;; lets look at addition; in normal mathematical terms, this would look like
;; (first + x1) + x0
;; (first + x0) + x1
;; these must both be the same. 