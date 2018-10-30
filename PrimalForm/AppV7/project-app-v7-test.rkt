#lang s-exp "project-app-v7.rkt"

(require rackunit)


(define randomize-primal
  (λ (p)
    (sort p (λ (e1 e2) (< (random) 0.5)))))

;new data type tests
(check-equal? (¬ 2 ^ 4 3 5)
              '((-1 . 1) (2 . 4) (3 . 1) (5 . 1)))
(check-equal? (¬ 2 ^ 4 3 5)
              '((-1 . 1) (2 . 4) (3 . 1) (5 . 1)))
(check-equal? (¬ 2 ^ 4 : 3 : 5)
              '((-1 . 1) (2 . 4) (3 . 1) (5 . 1)))
(check-equal? (¬ 2 ^ 4 : 3 : 5 :)
              '((-1 . 1) (2 . 4) (3 . 1) (5 . 1)))
(check-equal? (¬ 2 ^ 4 : 3 : 5 :)
              '((-1 . 1) (2 . 4) (3 . 1) (5 . 1)))
(check-exn exn:fail? (λ () (¬ 7 : ^ 2)))


;primal?
(check-true (primal? (zero)))
(check-true (primal? ()))
(check-true (primal? (1 ^ 7)))
(check-true (primal? (2 ^ 5 : 3 ^ 90)))
(check-true (primal? (3 ^ 90 : 2 ^ 5)))
(check-false (primal? (list (cons 3 90) (cons 2 5))))
(check-true (primal? (2 ^ 5 : 3 ^ 90)))
(check-exn exn:fail? (λ () (primal? (2 ^ 5 : 4 ^ 90))))

;primal-zero?
(check-true (primal-zero? (zero)))
(check-false (primal-zero? ()))
(check-false (primal-zero? (1 ^ 1)))
(check-false (primal-zero? (1 ^ 7)))
(check-false (primal-zero? (3 ^ 7)))

;primal=?
(check-true (primal=? (2 ^ 1 : 3 ^ 2 : 7 ^ 1)
                      (2 ^ 1 : 7 ^ 1 : 3 ^ 2)))
(check-true (primal=? (1 ^ 7 : 2 ^ 1 : 3 ^ 2 : 7 ^ 1)
                      (2 ^ 1 : 7 ^ 1 : 3 ^ 2)))
(check-false (primal=? (1 ^ 7 : 2 ^ 1 : 3 ^ 2 : 7 ^ 1)
                       (2 ^ 1 : 7 ^ 1 : 3 ^ 1)))

;primal->integer
(check-equal? (primal->integer (zero))
              0)
(check-equal? (primal->integer ())
              1)
(check-equal? (primal->integer (1 ^ 1))
              1)
(check-equal? (primal->integer (1 ^ 7))
              1)
(check-equal? (primal->integer (3 ^ 7 : 5 ^ 3))
              273375)
(check-equal? (primal->integer (neg 3 ^ 7 : 5 ^ 3))
              -273375)
(check-equal? (primal->integer (¬ 3 ^ 7 : 5 ^ 3))
              -273375)

;disjoint
(check-true (disjoint? (2 ^ 1 : 3 ^ 2 : 7 ^ 1)
                       (13 ^ 1 : 17 ^ 3 : 23 ^ 78)))
(check-true (disjoint? (13 ^ 1 : 17 ^ 3 : 23 ^ 78)
                       (2 ^ 1 : 3 ^ 2 : 7 ^ 1)))
(check-false (disjoint? (3 ^ 43 : 13 ^ 1 : 17 ^ 3 : 23 ^ 78)
                        (2 ^ 1 : 3 ^ 2 : 7 ^ 1)))
(check-false (disjoint? (2 ^ 1 : 3 ^ 2 : 7 ^ 1)
                        (3 ^ 43 : 13 ^ 1 : 17 ^ 3 : 23 ^ 78)))

;cardinality
(check-equal? (cardinality (zero))
              0)
(check-equal? (cardinality ())
              0)
(check-equal? (cardinality (1 ^ 1))
              0)
(check-equal? (cardinality (1 ^ 7))
              0)
(check-equal? (cardinality (2 ^ 1 : 3 ^ 2 : 7 ^ 1))
              3)

;intersection
(check-equal? (intersection (list (cons 2 4) (cons 3 7)) (list (cons 2 1) (cons 3 2) (cons 7 1)))
              '((2 . 1) (3 . 2)))
(check-equal? (intersection (2 ^ 4 : 3 ^ 7)
                            (2 ^ 1 : 3 ^ 2 : 7 ^ 1))
              '((2 . 1) (3 . 2)))
(check-equal? (∩ (2 ^ 4 : 3 ^ 7)
                 (2 ^ 1 : 3 ^ 2 : 7 ^ 1))
              '((2 . 1) (3 . 2)))
(check-equal? (∩ (2 ^ 4 : 3 ^ 7)
                 (2 ^ 1 : 3 ^ 2 : 7 ^ 1)
                 (3 ^ 1 : 7 ^ 13))
              '((3 . 1)))
(check-equal? (∩ (2 ^ 1 : 3 ^ 2 : 7 ^ 1) (13 ^ 1 : 17 ^ 3 : 23 ^ 78))
              '())
(check-equal? (∩ (zero) (zero))
              (zero))
(check-equal? (∩ (3 ^ 7) (zero))
              (zero))
(check-equal? (∩ (zero) (3 ^ 7))
              (zero))

;union
(check-equal? (union (2 ^ 4 : 3 ^ 5)
                     (7 ^ 3))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (∪ (2 ^ 4 : 3 ^ 5)
                 (7 ^ 3))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (union (7 ^ 3)
                     (2 ^ 4 : 3 ^ 5))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (∪ (7 ^ 3)
                 (2 ^ 4 : 3 ^ 5))
              '((2 . 4) (3 . 5) (7 . 3)))
(check-equal? (∪ (7 ^ 3)
                 (2 ^ 4 : 3 ^ 5)
                 (2 ^ 13 : 3 ^ 5 : 11 ^ 2))
              '((2 . 13) (3 . 5) (7 . 3) (11 . 2)))
(check-equal? (∪ (zero) (zero))
              (zero))
(check-equal? (∪ (3 ^ 7) (zero))
              (3 ^ 7))
(check-equal? (∪ (zero) (3 ^ 7))
              (3 ^ 7))

;subtraction
(check-equal? (subtract (1 ^ 3) (1 ^ 4))
              '())
(check-equal? (// (1 ^ 3) (1 ^ 4))
              '())
(check-equal? (// (3 ^ 2 : 2 ^ 2)
                  (2 ^ 2 : 3 ^ 2))
              '())
(check-equal? (subtract (1 ^ 7 : 3 ^ 4 : 7 ^ 3) (1 ^ 6 : 3 ^ 2 : 7 ^ 4))
              '((3 . 2)))
(check-equal? (// (1 ^ 7 : 3 ^ 4 : 7 ^ 3) (1 ^ 6 : 3 ^ 2 : 7 ^ 4))
              '((3 . 2)))
(check-equal? (// (zero) (zero))
              (zero))
(check-equal? (// (3 ^ 7) (zero))
              (3 ^ 7))
(check-equal? (// (zero) (3 ^ 7))
              (zero))

;add
(check-equal? (add (1 ^ 7) (2 ^ 3 : 11 ^ 3) (5 ^ 1))
              '((2 . 3) (5 . 1) (11 . 3)))
(check-equal? (add (2 ^ 3 : 11 ^ 3) ())
              (2 ^ 3 : 11 ^ 3))
(check-equal? (add () (2 ^ 3 : 11 ^ 3))
              (2 ^ 3 : 11 ^ 3))

;partition
(check-true (partition? (3 ^ 7 : 2 ^ 7) (3 ^ 3) (3 ^ 4 : 2 ^ 7)))
(check-false (partition? (3 ^ 7 : 2 ^ 7) (3 ^ 2) (3 ^ 4 : 2 ^ 7)))

;fun stuff
(define gcd ∩)

(define lcm 'todo)

(define primal-* ++)

(define primal-/ //)

(define prime?
  (λ (p)
    (or
     (null? p)
     (and
      (primal? p)
      (not (primal-zero? p))
      (zero? (sub1 (cardinality p)))
      (zero? (sub1 (cdar p)))))))

(check-true (prime? (7 ^ 1)))
(check-true (prime? (1 ^ 7)))
(check-false (prime? (3 ^ 1 : 7 ^ 1)))
(check-false (prime? (3 ^ 2)))
(check-false (prime? (zero)))

;trying out built in match
(check-equal? ((λ (p)
                 (match p
                   [(cons (cons 2 x) y) x]
                   [_ p]))
               (2 ^ 3 : 3 ^ 1 : 5 ^ 8 : 7 ^ 2))
              3)
(check-equal? ((λ (p)
                 (match p
                   [(cons (cons x 3) y) x]
                   [_ p]))
               (2 ^ 3 : 3 ^ 1 : 5 ^ 8 : 7 ^ 2))
              2)
