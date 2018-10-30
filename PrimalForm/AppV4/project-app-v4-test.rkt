#lang s-exp "project-app-v4.rkt"

(require rackunit)

;primal?
(check-equal? (primal? ())
              #t)
(check-equal? (primal? ((2 ^ 5) (3 ^ 90)))
              #t)
(check-equal? (primal? ((2 ^ 5) (4 ^ 90)))
              #f)

;primal=?
(check-true (primal=? ((2 ^ 1) (3 ^ 2) (7 ^ 1)) ((2 ^ 1) (7 ^ 1) (3 ^ 2))))
(check-true (primal=? ((1 ^ 7) (2 ^ 1) (3 ^ 2) (7 ^ 1)) ((2 ^ 1) (7 ^ 1) (3 ^ 2))))
(check-false (primal=? ((1 ^ 7) (2 ^ 1) (3 ^ 2) (7 ^ 1)) ((2 ^ 1) (7 ^ 1) (3 ^ 1))))

;primal->integer
(check-equal? (primal->integer (zero))
              0)
(check-equal? (primal->integer ())
              1)
(check-equal? (primal->integer ((1 ^ 1)))
              1)
(check-equal? (primal->integer ((1 ^ 7)))
              1)
(check-equal? (primal->integer ((3 ^ 7) (5 ^ 3)))
              273375)
(check-equal? (primal->integer (neg (3 ^ 7) (5 ^ 3)))
              -273375)
(check-equal? (primal->integer (¬ (3 ^ 7) (5 ^ 3)))
              -273375)

;disjoint
(check-true (disjoint? ((2 ^ 1) (3 ^ 2) (7 ^ 1)) ((13 ^ 1) (17 ^ 3) (23 ^ 78))))
(check-true (disjoint? ((13 ^ 1) (17 ^ 3) (23 ^ 78)) ((2 ^ 1) (3 ^ 2) (7 ^ 1))))
(check-false (disjoint? ((3 ^ 43) (13 ^ 1) (17 ^ 3) (23 ^ 78)) ((2 ^ 1) (3 ^ 2) (7 ^ 1))))
(check-false (disjoint? ((2 ^ 1) (3 ^ 2) (7 ^ 1)) ((3 ^ 43) (13 ^ 1) (17 ^ 3) (23 ^ 78))))

;cardinality
(check-equal? (cardinality ())
              0)
(check-equal? (cardinality ((2 ^ 1) (3 ^ 2) (7 ^ 1)))
              3)

;intersection
(check-equal? (intersection (list (cons 2 4) (cons 3 7)) (list (cons 2 1) (cons 3 2) (cons 7 1)))
              '((2 . 1) (3 . 2)))
(check-equal? (intersection ((2 ^ 4) (3 ^ 7))
                            ((2 ^ 1) (3 ^ 2) (7 ^ 1)))
              '((2 . 1) (3 . 2)))
(check-equal? (∩ ((2 ^ 4) (3 ^ 7))
                 ((2 ^ 1) (3 ^ 2) (7 ^ 1)))
              '((2 . 1) (3 . 2)))
(check-equal? (∩ ((2 ^ 4) (3 ^ 7))
                 ((2 ^ 1) (3 ^ 2) (7 ^ 1))
                 ((3 ^ 1) (7 ^ 13)))
              '((3 . 1)))
(check-equal? (∩ ((2 ^ 1) (3 ^ 2) (7 ^ 1)) ((13 ^ 1) (17 ^ 3) (23 ^ 78)))
              '())

;union
(check-equal? (union ((2 ^ 4) (3 ^ 5))
                     ((7 ^ 3)))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (∪ ((2 ^ 4) (3 ^ 5))
                 ((7 ^ 3)))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (union ((7 ^ 3))
                     ((2 ^ 4) (3 ^ 5)))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (∪ ((7 ^ 3))
                 ((2 ^ 4) (3 ^ 5)))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (∪ ((7 ^ 3))
                 ((2 ^ 4) (3 ^ 5))
                 ((2 ^ 13) (3 ^ 5) (11 ^ 2)))
              '((2 . 13) (3 . 5) (7 . 3) (11 . 2)))

;subtraction
(check-equal? (subtract ((1 ^ 3)) ((1 ^ 4)))
              (zero))
(check-equal? (// ((1 ^ 3)) ((1 ^ 4)))
              (zero))
(check-equal? (subtract ((1 ^ 7) (3 ^ 4) (7 ^ 3)) ((1 ^ 6) (3 ^ 2) (7 ^ 4)))
              '((3 . 2)))
(check-equal? (// ((1 ^ 7) (3 ^ 4) (7 ^ 3)) ((1 ^ 6) (3 ^ 2) (7 ^ 4)))
              '((3 . 2)))

;add
(check-equal? (add ((1 ^ 7)) ((2 ^ 3) (11 ^ 3)) ((5 ^ 1)))
              '((2 . 3) (5 . 1) (11 . 3)))
(check-equal? (add ((2 ^ 3) (11 ^ 3)) ())
              ((2 ^ 3) (11 ^ 3)))
(check-equal? (add () ((2 ^ 3) (11 ^ 3)))
              ((2 ^ 3) (11 ^ 3)))

;partition
(check-true (partition? ((3 ^ 7) (4 ^ 7)) ((3 ^ 3)) ((3 ^ 4) (4 ^ 7))))
(check-false (partition? ((3 ^ 7) (4 ^ 7)) ((3 ^ 2)) ((3 ^ 4) (4 ^ 7))))

;fun stuff
(define gcd ∩)

(define primal-*
  (λ args
    (apply ++ args)))