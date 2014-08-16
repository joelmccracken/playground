#lang scheme
; this is the main library file for programming with SICP.
; it should have almost all requirements for creating somewhat-ideal code for sicp
; this file should be a guide of sorts, explaining in some way why 
; this all was necessary. Anyway, any suggestions are appreciated. 

; this file should also include any code that he provides within the sections.
; so you will not need to copy/paste the book's code.

; goals of the library:
; 1. never have to go back to old code in order to do a new section; that is, 
;    not to have to worry about dealing with code management
; 2. not to have to copy his code into your files in multiple places in order
;    to continue with the exercises


; to use this library:
; the suggestion is to create your own answers directories, and then
; in each answer file include a
; (require (only-in "../lib.ss" bindings...))
; so for example, if I only want to include nil, I might use 
; (require (only-in "../lib.ss" nil))


;;; and here is the body of the library


(require "lib/accumulate.ss"
         "lib/dot-product.ss"
         "lib/accumulate-n.ss"
         "lib/corner-split.ss"
         "lib/enumerate-interval.ss"
         "lib/flatmap.ss"
         "lib/fold-left.ss"
         "lib/make-pair-sum.ss"
         "lib/nil.ss"
         "lib/prime.ss"
         "lib/prime-sum.ss"
         "lib/right-split.ss"
         "lib/up-split.ss"
         "lib/derivative.ss"
         "lib/sets.ss"
         )

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


; sicp uses nil for a long time until it explains '()
(define fold-right accumulate)

(provide accumulate
         accumulate-n
         corner-split
         dot-product
         enumerate-interval
         flatmap
         fold-left
         fold-right
         make-pair-sum
         nil
         prime?
         prime-sum?
         paint number->painter diagonal-shading 
         paint-hires below beside rotate90 rotate270 rotate180 einstein
         transform-painter
         right-split
         up-split
         segments->painter
         make-segment
         make-vect
         flip-vert
         ; derivative stuff
         deriv number? variable? sum? exponentiation? base exponent make-exponentiation same-variable? make-sum =number? make-product addend augend
         product? multiplier multiplicand
         element-of-set? adjoin-set intersection-set
         )