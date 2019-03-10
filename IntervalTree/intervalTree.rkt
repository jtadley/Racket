#lang racket

(require racket/struct)

(struct node
  (low
   high
   max
   left
   right)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (n) 'node)
      (λ (n) (list (node-low n)
                   (node-high n)
                   (node-max n)
                   (if (node-left n)
                       #t
                       #f)
                   (if (node-right n)
                       #t
                       #f)))))])