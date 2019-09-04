#lang racket

(require 2htdp/image
         lang/posn
         racket/struct)

(define OVAL "oval")
(define DIAMOND "diamond")
(define SQUIGGLE "squiggle")
(define ONE 1)
(define TWO 2)
(define THREE 3)
(define SOLID "solid")
(define OUTLINE "outline")
(define PATTERN "pattern")
(define RED "red")
(define GREEN "green")
(define PURPLE "purple")

(struct card
  (posn ; lang/posn
   shape ; oval | diamond | squiggle
   number ; 1 | 2 | 3
   fill ; solid | outline | pattern
   color ; red | green | purple
   )
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (c) 'card)
      (λ (c) (list
              (card-posn c)
              (card-shape c)
              (card-number c)
              (card-fill c)
              (card-color c)))))])

(define card-equal?
  (λ (c1 c2)
    (equal? (card-posn c1) (card-posn c2))))

(define all-different-cards?
  (λ (c1 c2 c3)
    (and
     (not (card-equal? c1 c2))
     (not (card-equal? c2 c3))
     (not (card-equal? c3 c1)))))

(define all-same?
  (λ (x y z)
    (and (equal? x y)
         (equal? y z))))

(define all-different?
  (λ (x y z)
    (and (not (equal? x y))
         (not (equal? y z))
         (not (equal? z x)))))

(define all-same/different?
  (λ (x y z)
    (or (all-same? x y z)
        (all-different? x y z))))

(define is-shape-set?
  (λ (c1 c2 c3)
    (let* ([c1-shape (card-shape c1)]
           [c2-shape (card-shape c2)]
           [c3-shape (card-shape c3)])
      (all-same/different? c1-shape c2-shape c3-shape))))

(define is-number-set?
  (λ (c1 c2 c3)
    (let* ([c1-number (card-number c1)]
           [c2-number (card-number c2)]
           [c3-number (card-number c3)])
      (all-same/different? c1-number c2-number c3-number))))

(define is-fill-set?
  (λ (c1 c2 c3)
    (let* ([c1-fill (card-fill c1)]
           [c2-fill (card-fill c2)]
           [c3-fill (card-fill c3)])
      (all-same/different? c1-fill c2-fill c3-fill))))

(define is-color-set?
  (λ (c1 c2 c3)
    (let* ([c1-color (card-color c1)]
           [c2-color (card-color c2)]
           [c3-color (card-color c3)])
      (all-same/different? c1-color c2-color c3-color))))

(define is-set?
  (λ (c1 c2 c3)
    (and
     (is-shape-set? c1 c2 c3)
     (is-number-set? c1 c2 c3)
     (is-fill-set? c1 c2 c3)
     (is-color-set? c1 c2 c3))))

(define all-pairs
  (λ (loc)
    (letrec
        ([helper
          (λ (l1 l2 l3)
            (cond
              [(null? l1) '()]
              [(null? l2) (helper (cdr l1) loc loc)]
              [(null? l3) (helper l1 (cdr l2) loc)]
              [else (if (all-different-cards? (car l1) (car l2) (car l3))
                        (cons (list (car l1) (car l2) (car l3))
                              (helper l1 l2 (cdr l3)))
                        (helper l1 l2 (cdr l3)))]))])
      (helper loc loc loc))))

;; List of Card -> Boolean
;; Is a set possible in the given list of card
(define set-exists?
  (λ (loc)
    (letrec
        ; List of List of Card -> Boolean
        ; Given a list of list of 3 cards, do any of the lists constitute a set?
        ([helper
          (λ (lop)
            (cond
              [(null? lop) #f]
              [else (let* ([c1 (caar lop)]
                           [c2 (cadar lop)]
                           [c3 (caddar lop)])
                      (or (if (is-set? c1 c2 c3)
                              (list c1 c2 c3)
                              #f)
                          (helper (cdr lop))))]))])
      (helper (all-pairs loc)))))

(define c1
  (card (make-posn 0 0)
        SQUIGGLE
        TWO
        PATTERN
        PURPLE))
(define c2
  (card (make-posn 0 1)
        SQUIGGLE
        THREE
        PATTERN
        PURPLE))
(define c3
  (card (make-posn 0 2)
        SQUIGGLE
        ONE
        SOLID
        PURPLE))
(define c4
  (card (make-posn 0 3)
        SQUIGGLE
        TWO
        OUTLINE
        PURPLE))
(define c5
  (card (make-posn 0 4)
        SQUIGGLE
        TWO
        PATTERN
        RED))
(define c6
  (card (make-posn 0 5)
        SQUIGGLE
        TWO
        PATTERN
        GREEN))
(define c7
  (card (make-posn 0 6)
        OVAL
        TWO
        PATTERN
        PURPLE))

(define loc (list c1 c2 c3 c4 c5 c6 c7))
(set-exists? loc)