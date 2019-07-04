#lang racket

(define reverse
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(not (list? (car ls)))
       (reverse (cons (cdr ls)
                      (cons (car ls) '())))]
      [(null? (car ls)) (cdr ls)]
      [else (reverse (cons (cdr (car ls))
                           (cons (car (car ls)) (cdr ls))))])))

(reverse '(1 2 3))
