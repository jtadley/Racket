#lang racket

(require 2htdp/image
         2htdp/universe
         lang/posn
         racket/struct)


(define CARD-COLOR (make-color 255 245 238))

                                                                                             
;                                                                                                  
;                                                                                                  
;     ;;;;          ;;;;             ;;;;              ;;;;;;;;;;;;;;;;;        ;;;;;;;;;;;;;;;;   
;     ;;;;          ;;;;             ;;;;              ;;;;;;;;;;;;;;;;;        ;;;;;;;;;;;;;;;;   
;     ;;;;;        ;;;;;            ;;;;;;             ;;;;;;;;;;;;;;;;;        ;;;;;;;;;;;;;;;;   
;     ;;;;;        ;;;;;            ;;;;;;                          ;;;         ;;;                
;     ;;;;;        ;;;;;            ;; ;;;                         ;;;;         ;;;                
;     ;;;;;;      ;;;;;;           ;;;  ;;;                       ;;;;          ;;;                
;     ;;;;;;      ;;;;;;           ;;;  ;;;                       ;;;           ;;;                
;     ;;; ;;     ;;; ;;;           ;;   ;;;                      ;;;;           ;;;                
;     ;;; ;;;    ;;; ;;;          ;;;    ;;;                    ;;;;            ;;;                
;     ;;; ;;;    ;;; ;;;          ;;;    ;;;                    ;;;             ;;;                
;     ;;;  ;;   ;;;  ;;;          ;;;    ;;;                   ;;;              ;;;                
;     ;;;  ;;;  ;;;  ;;;         ;;;      ;;;                 ;;;;              ;;;;;;;;;;;;;;     
;     ;;;  ;;;  ;;;  ;;;         ;;;      ;;;                 ;;;               ;;;;;;;;;;;;;;     
;     ;;;   ;;  ;;   ;;;         ;;;      ;;;                ;;;                ;;;;;;;;;;;;;;     
;     ;;;   ;;;;;;   ;;;        ;;;        ;;;              ;;;;                ;;;                
;     ;;;   ;;;;;    ;;;        ;;;;;;;;;;;;;;             ;;;;                 ;;;                
;     ;;;    ;;;;    ;;;       ;;;;;;;;;;;;;;;             ;;;                  ;;;                
;     ;;;    ;;;;    ;;;       ;;;;;;;;;;;;;;;;           ;;;;                  ;;;                
;     ;;;     ;;     ;;;       ;;;          ;;;          ;;;;                   ;;;                
;     ;;;            ;;;      ;;;;          ;;;;         ;;;                    ;;;                
;     ;;;            ;;;      ;;;           ;;;;        ;;;;                    ;;;                
;     ;;;            ;;;      ;;;            ;;;       ;;;;                     ;;;                
;     ;;;            ;;;     ;;;;            ;;;;      ;;;                      ;;;                
;     ;;;            ;;;     ;;;             ;;;;     ;;;;;;;;;;;;;;;;;;;       ;;;;;;;;;;;;;;;;   
;     ;;;            ;;;     ;;;              ;;;     ;;;;;;;;;;;;;;;;;;;       ;;;;;;;;;;;;;;;;   
;     ;;;            ;;;    ;;;;              ;;;;    ;;;;;;;;;;;;;;;;;;;       ;;;;;;;;;;;;;;;;   
;                                                                                                  
(define MAZE-WIDTH 400)
(define MAZE-HEIGHT 400)
(define MAZE-SIZE 5)
(define MAZE-OFFSET (* 10 MAZE-SIZE))
(define BG_COLOR CARD-COLOR)
(define PEN (make-pen "cyan" (/ MAZE-SIZE 5) "solid" "round" "miter"))

(define tmp (scene+polygon
             (empty-scene MAZE-WIDTH MAZE-HEIGHT)
             (list (make-posn 49 49)
                   (make-posn (+ 49 2 (* 10 MAZE-SIZE)) 49)
                   (make-posn (+ 49 2 (* 10 MAZE-SIZE)) (+ 49 2 (* 10 MAZE-SIZE)))
                   (make-posn 49 (+ 49 2 (* 10 MAZE-SIZE))))
             "outline"
             "red"))
(define tmp_vert
  (λ ()
    (draw_block_vertical 50 50 tmp)))
(define tmp_horz
  (λ ()
    (draw_block_horizontal 50 50 tmp)))

(define draw_block_vertical
  (λ (x y img)
    (scene+line ; bottom-right vertical bar
     (scene+line ; bottom-left vertical bar
      (scene+line ; top-right vertical bar
       (scene+line ; top-left vertical bar
        (scene+line ; middle vertical bar
         (scene+line ; horizontal bottom bar
          (scene+line ; horizontal top bar
           img
           x
           y
           (+ x (* 6 MAZE-SIZE))
           y
           PEN)
          x
          (+ y (* 8 MAZE-SIZE))
          (+ x (* 6 MAZE-SIZE))
          (+ y (* 8 MAZE-SIZE))
          PEN)
         (+ x (* 3 MAZE-SIZE))
         y
         (+ x (* 3 MAZE-SIZE))
         (+ y (* 8 MAZE-SIZE))
         PEN)
        (+ x MAZE-SIZE)
        y
        (+ x MAZE-SIZE)
        (+ y MAZE-SIZE)
        PEN)
       (+ x (* 5 MAZE-SIZE))
       y
       (+ x (* 5 MAZE-SIZE))
       (+ y MAZE-SIZE)
       PEN)
      (+ x MAZE-SIZE)
      (+ y (* 7 MAZE-SIZE))
      (+ x MAZE-SIZE)
      (+ y (* 8 MAZE-SIZE))
      PEN)
     (+ x (* 5 MAZE-SIZE))
     (+ y (* 7 MAZE-SIZE))
     (+ x (* 5 MAZE-SIZE))
     (+ y (* 8 MAZE-SIZE))
     PEN)))

(define draw_block_horizontal
  (λ (x y img)
    (scene+line
     (scene+line
      (scene+line
       (scene+line
        (scene+line ; middle horizontal bar
         (scene+line ; vertical right bar
          (scene+line ; verical left bar
           img
           y
           x
           y
           (+ x (* 6 MAZE-SIZE))
           PEN)
          (+ y (* 8 MAZE-SIZE))
          x
          (+ y (* 8 MAZE-SIZE))
          (+ x (* 6 MAZE-SIZE))
          PEN)
         y
         (+ x (* 3 MAZE-SIZE))
         (+ y (* 8 MAZE-SIZE))
         (+ x (* 3 MAZE-SIZE))
         PEN)
        y
        (+ x MAZE-SIZE)
        (+ y MAZE-SIZE)
        (+ x MAZE-SIZE)
        PEN)
       y
       (+ x (* 5 MAZE-SIZE))
       (+ y MAZE-SIZE)
       (+ x (* 5 MAZE-SIZE))
       PEN)
      (+ y (* 7 MAZE-SIZE))
      (+ x MAZE-SIZE)
      (+ y (* 8 MAZE-SIZE))
      (+ x MAZE-SIZE)
      PEN)
     (+ y (* 7 MAZE-SIZE))
     (+ x (* 5 MAZE-SIZE))
     (+ y (* 8 MAZE-SIZE))
     (+ x (* 5 MAZE-SIZE))
     PEN)))

(define draw_maze
  (λ (x y)
    (cond
      [(>= y (+ MAZE-HEIGHT MAZE-OFFSET))
       (rectangle MAZE-WIDTH MAZE-HEIGHT "solid" BG_COLOR)]
      [(>= x (+ MAZE-WIDTH MAZE-OFFSET))
       (draw_maze (- 0 MAZE-OFFSET) (add1 y))]
      [(or
        (and
         (zero? (modulo x (* 10 MAZE-SIZE)))
         (zero? (modulo y (* 10 MAZE-SIZE))))
        (and
         (eqv? (modulo x (* 10 MAZE-SIZE)) (* 5 MAZE-SIZE))
         (eqv? (modulo y (* 10 MAZE-SIZE)) (* 5 MAZE-SIZE))))
       (draw_block_vertical x y (draw_maze (add1 x) y))]
      [(or
        (and
         (eqv? (modulo x (* 10 MAZE-SIZE)) (* 4 MAZE-SIZE))
         (eqv? (modulo y (* 10 MAZE-SIZE)) MAZE-SIZE))
        (and
         (eqv? (modulo x (* 10 MAZE-SIZE)) (* 9 MAZE-SIZE))
         (eqv? (modulo y (* 10 MAZE-SIZE)) (* 6 MAZE-SIZE))))
       (draw_block_horizontal y x (draw_maze (add1 x) y))]
      [else (draw_maze (add1 x) y)])))

;(draw_maze (- 0 MAZE-OFFSET) (- 0 MAZE-OFFSET))



;                                                                                                                              
;           ;;;;;;;             ;;;;;;;;;;;;;;;;     ;;;;;;;;;;;;;;;;;;;;  
;         ;;;;;;;;;;;           ;;;;;;;;;;;;;;;;     ;;;;;;;;;;;;;;;;;;;;  
;        ;;;;;;;;;;;;;;         ;;;;;;;;;;;;;;;;     ;;;;;;;;;;;;;;;;;;;;  
;       ;;;;       ;;;;         ;;;                          ;;;;          
;      ;;;;          ;;         ;;;                          ;;;;          
;      ;;;                      ;;;                          ;;;;          
;      ;;;                      ;;;                          ;;;;          
;      ;;;                      ;;;                          ;;;;          
;      ;;;;                     ;;;                          ;;;;          
;       ;;;;                    ;;;                          ;;;;          
;       ;;;;;;;                 ;;;                          ;;;;          
;        ;;;;;;;;               ;;;;;;;;;;;;;;               ;;;;          
;          ;;;;;;;;             ;;;;;;;;;;;;;;               ;;;;          
;            ;;;;;;;;           ;;;;;;;;;;;;;;               ;;;;          
;               ;;;;;;;         ;;;                          ;;;;          
;                 ;;;;;;        ;;;                          ;;;;          
;                   ;;;;        ;;;                          ;;;;          
;                    ;;;        ;;;                          ;;;;          
;                    ;;;        ;;;                          ;;;;          
;                    ;;;        ;;;                          ;;;;          
;      ;             ;;;        ;;;                          ;;;;          
;     ;;;;          ;;;;        ;;;                          ;;;;          
;     ;;;;;;       ;;;;         ;;;                          ;;;;          
;      ;;;;;;;;;;;;;;;          ;;;;;;;;;;;;;;;;             ;;;;          
;        ;;;;;;;;;;;;           ;;;;;;;;;;;;;;;;             ;;;;          
;           ;;;;;;;             ;;;;;;;;;;;;;;;;             ;;;;          
;                                                                                                                                          


(define FRAME-HEIGHT 1000)
(define FRAME-WIDTH 1000)
(define FRAME-COLOR (make-color 255 255 255))
(define FRAME (empty-scene FRAME-WIDTH FRAME-HEIGHT FRAME-COLOR))
(define GAP 40)
(define CARD-HEIGHT 200)
(define CARD-WIDTH 280)
(define MAX-X 2)
(define MAX-Y 3)

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

(define PEN-SIZE 3)
(define PURPLE-COLOR (make-color 139 0 139))
(define PURPLE-PEN (make-pen PURPLE-COLOR PEN-SIZE "solid" "round" "miter"))
(set! PEN PURPLE-PEN)
(define PURPLE-MAZE (draw_maze (- 0 MAZE-OFFSET) (- 0 MAZE-OFFSET)))
(define RED-COLOR (make-color 139 0 0))
(define RED-PEN (make-pen RED-COLOR PEN-SIZE "solid" "round" "miter"))
(set! PEN RED-PEN)
(define RED-MAZE (draw_maze (- 0 MAZE-OFFSET) (- 0 MAZE-OFFSET)))
(define GREEN-COLOR (make-color 46 139 87))
(define GREEN-PEN (make-pen GREEN-COLOR PEN-SIZE "solid" "round" "miter"))
(set! PEN GREEN-PEN)
(define GREEN-MAZE (draw_maze (- 0 MAZE-OFFSET) (- 0 MAZE-OFFSET)))

(define DIAMOND-SIZE-LEN (/ CARD-HEIGHT 3))
(define OVAL-WIDTH 51 #;DIAMOND-SIZE-LEN)
(define OVAL-HEIGHT 123 #;(* (sqrt (+ (expt DIAMOND-SIZE-LEN 2) (expt (/ DIAMOND-SIZE-LEN 2) 2))) 2))
(define SQUIGGLE-POINTS (list (make-pulled-point 1/2 -25 0 0 1/2 25)
                              (make-posn 20 -30)
                              (make-pulled-point 1/2 25 0 60 1/2 -25)
                              (make-posn -20 -30)))
(define SQUIGGLE-OUTLINE-POINTS (list (make-pulled-point 1/2 -25 -20 -20 1/2 25)
                                      (make-posn 40 -50)
                                      (make-pulled-point 1/2 25 -20 80 1/2 -25)
                                      (make-posn -40 -50)))

(struct card
  (posn ; lang/posn
   shape ; oval | diamond | squiggle
   number ; 1 | 2 | 3
   fill ; solid | outline | pattern
   color ; red | green | purple
   selected ; #t | #f
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
              (card-color c)
              (card-selected c)))))])

(define make-card
  (λ (p s n f c slctd)
    (card p s n f c slctd)))

(define select-card
  (λ (c)
    (let ([p (card-posn c)]
          [s (card-shape c)]
          [n (card-number c)]
          [f (card-fill c)]
          [c (card-color c)])
      (make-card p s n f c #t))))

(define unselect-card
  (λ (c)
    (let ([p (card-posn c)]
          [s (card-shape c)]
          [n (card-number c)]
          [f (card-fill c)]
          [c (card-color c)])
      (make-card p s n f c #f))))

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

(define get-card-pen/color
  (λ (c)
    (match (list (card-color c) (card-fill c))
      ['("red" "solid") RED-COLOR]
      [`("red" ,_) RED-PEN]
      ['("green" "solid") GREEN-COLOR]
      [`("green" ,_) GREEN-PEN]
      ['("purple" "solid") PURPLE-COLOR]
      [`("purple" ,_) PURPLE-PEN])))

(define get-card-maze
  (λ (c)
    (match (card-color c)
      ["red" RED-MAZE]
      ["green" GREEN-MAZE]
      ["purple" PURPLE-MAZE])))

(define add0.5
  (λ (x) (+ x 0.5)))

(define add2
  (λ (x) (add1 (add1 x))))

(define add3
  (λ (x) (add1 (add2 x))))

(define trim-diamond
  (λ (width height scene)
    (let* ([pen-size/2 (add0.5 (/ PEN-SIZE 2))]
           [width/2 (/ width 2)]
           [height/2 (/ height 2)]
           [top-left-cleared
            (add-polygon ;top-left
             scene
             (list (make-posn -1 -1)
                   (make-posn -0.5 (- height/2 pen-size/2))
                   (make-posn (- width/2 pen-size/2) -0.5))
             SOLID
             CARD-COLOR)]
           [top-cleared
            (add-polygon
             top-left-cleared
             (list (make-posn (add2 width) -1)
                   (make-posn (+ width/2 pen-size/2) -0.5)
                   (make-posn (add2 width) (- height/2 pen-size/2)))
             SOLID
             CARD-COLOR)]
           [top-bottom-left-cleared
            (add-polygon
             top-cleared
             (list (make-posn -0.5 (add3 height))
                   (make-posn -0.5 (+ height/2 pen-size/2))
                   (make-posn width/2 (add2 height)))
             SOLID
             CARD-COLOR)]
           [all-cleared
            (add-polygon
             top-bottom-left-cleared
             (list (make-posn (add3 width) (add3 height))
                   (make-posn (+ width/2 pen-size/2) (add3 height))
                   (make-posn (add2 width) (+ height/2 pen-size/2)))
             SOLID
             CARD-COLOR)])
      all-cleared)))

(define draw-diamond
  (λ (c x y fill scene)
    (let* ([pen/color (get-card-pen/color c)])
      (match fill
        ["pattern" (let* ([r (rhombus DIAMOND-SIZE-LEN 45 OUTLINE pen/color)]
                          [r-width (image-width r)]
                          [r-height (image-height r)]
                          [r-width/2 (/ r-width 2)]
                          [r-height/2 (/ r-height 2)]
                          [diamond (trim-diamond
                                    r-width
                                    r-height
                                    (place-image
                                     r
                                     r-width/2
                                     r-height/2
                                     (place-image
                                      (get-card-maze c)
                                      r-width/2
                                      r-height/2
                                      r)))])
                     (place-image
                      diamond
                      x
                      y
                      scene))]
        [_
         (place-image
          (rhombus DIAMOND-SIZE-LEN 45 fill pen/color)
          x
          y
          scene)]))))

(define trim-oval
  (λ (width height scene)
    (let* ([pen (make-pen CARD-COLOR 20 "solid" "round" "miter")]
           [outlined-oval (place-image
                           (ellipse (+ 25 OVAL-WIDTH) (+ 25 OVAL-HEIGHT) OUTLINE pen)
                           (/ OVAL-WIDTH 2)
                           (/ OVAL-HEIGHT 2)
                           scene)]
           [removed-ttl outlined-oval])
      removed-ttl)))

(define draw-oval
  (λ (c x y fill scene)
    (let* ([pen/color (get-card-pen/color c)])
      (match fill
        ["pattern" (let* ([e (ellipse OVAL-WIDTH OVAL-HEIGHT OUTLINE pen/color)]
                          [e-width (image-width e)]
                          [e-height (image-height e)]
                          [e-width/2 (/ e-width 2)]
                          [e-height/2 (/ e-height 2)]
                          [untrimmed-oval (place-image
                                           e
                                           e-width/2
                                           e-height/2
                                           (place-image
                                            (get-card-maze c)
                                            e-width/2
                                            e-height/2
                                            e))]
                          [trimmed-oval (trim-oval e-width e-height untrimmed-oval)])
                     (place-image
                      trimmed-oval
                      x
                      y
                      scene))]
        [_ (place-image
            (ellipse OVAL-WIDTH OVAL-HEIGHT fill pen/color)
            x
            y
            scene)]))))

(define trim-squiggle
  (λ (width height scene)
    (let* ([pen (make-pen CARD-COLOR 20 "solid" "round" "miter")]
           [outlined-squiggle (place-image
                               (polygon SQUIGGLE-OUTLINE-POINTS OUTLINE pen)
                               (/ width 2)
                               (/ height 2)
                               scene)])
      outlined-squiggle)))

(define draw-squiggle
  (λ (c x y fill scene)
    (let* ([pen/color (get-card-pen/color c)])
      (match fill
        ["pattern" (let* ([s (polygon SQUIGGLE-POINTS OUTLINE pen/color)]
                          [s-width (image-width s)]
                          [s-height (image-height s)]
                          [s-width/2 (/ s-width 2)]
                          [s-height/2 (/ s-height 2)]
                          [untrimmed-squiggle (place-image
                                               s
                                               s-width/2
                                               s-height/2
                                               (place-image
                                                (get-card-maze c)
                                                s-width/2
                                                s-height/2
                                                s))]
                          [trimmed-squiggle (trim-squiggle s-width s-height untrimmed-squiggle)])
                     (place-image
                      trimmed-squiggle
                      x
                      y
                      scene))]
        [_ (place-image
            (polygon SQUIGGLE-POINTS
                     fill
                     pen/color)
            x
            y
            scene)]))))

(define get-card-drawer
  (λ (c)
    (match (card-shape c)
      ["diamond" draw-diamond]
      ["oval" draw-oval]
      ["squiggle" draw-squiggle])))

(define draw-card
  (λ (c scene)
    (let* ([c-shape (card-shape c)]
           [c-number (card-number c)]
           [c-fill (card-fill c)]
           [p (card-posn c)]
           [p-x (posn-x p)]
           [p-y (posn-y p)]
           [x (+ (+ (* GAP (add1 p-x)) (* CARD-WIDTH p-x)) (/ CARD-WIDTH 2))]
           [y (+ (+ (* GAP (add1 p-y)) (* CARD-HEIGHT p-y)) (/ CARD-HEIGHT 2))]
           [shape-drawer (get-card-drawer c)]
           [empty-card (place-image
                        (rectangle CARD-WIDTH CARD-HEIGHT "solid" CARD-COLOR)
                        x
                        y
                        scene)])
      (match c-number
        [1 (shape-drawer c x y c-fill empty-card)]
        [2 (shape-drawer
            c (+ x (/ DIAMOND-SIZE-LEN 2)) y c-fill
            (shape-drawer c (- x (/ DIAMOND-SIZE-LEN 2)) y c-fill empty-card))]
        [3 (shape-drawer
            c (+ x DIAMOND-SIZE-LEN) y c-fill
            (shape-drawer
             c x y c-fill
             (shape-drawer c (- x DIAMOND-SIZE-LEN) y c-fill empty-card)))]))))

(define draw-world
  (λ (loc)
    (cond
      [(null? loc) FRAME]
      [else (draw-card (car loc)
                       (draw-world (cdr loc)))])))

(define get-random-card
  (λ (x y)
    (letrec ([get-random-123
              (λ ()
                (let ([rand (random)])
                  (cond
                    [(< rand (/ 1 3)) 1]
                    [(< rand (/ 2 3)) 2]
                    [else 3])))]
             [random-shape
              (let ([rand (get-random-123)])
                (match rand
                  [1 DIAMOND]
                  [2 OVAL]
                  [3 SQUIGGLE]))]
             [random-number
              (get-random-123)]
             [random-fill
              (let ([rand (get-random-123)])
                (match rand
                  [1 SOLID]
                  [2 OUTLINE]
                  [3 PATTERN]))]
             [random-color
              (let ([rand (get-random-123)])
                (match rand
                  [1 RED]
                  [2 GREEN]
                  [3 PURPLE]))])
      (make-card
       (make-posn x y)
       random-shape
       random-number
       random-fill
       random-color
       #f))))

;; takes a list of cards, returns if a card has a posn equal to (x,y)
(define x-y-card-in-world?
  (λ (x y loc)
    (cond
      [(null? loc) #f]
      [else (let* ([a (card-posn (car loc))]
                   [d (cdr loc)]
                   [a-x (posn-x a)]
                   [a-y (posn-y a)])
              (or (and
                   (= x a-x)
                   (= y a-y))
                  (x-y-card-in-world? x y d)))])))

;; take a list of cards, returns a list of cards such that all posns are filled and a set is possible
(define fill-world
  (λ (loc)
    (letrec ([helper
              (λ (x y)
                (cond
                  [(> y MAX-Y) loc]
                  [(> x MAX-X) (helper 0 (add1 y))]
                  [(x-y-card-in-world? x y loc) (helper (add1 x) y)]
                  [else (cons (get-random-card x y)
                              (helper (add1 x) y))]))])
      (let ([ans (helper 0 0)])
        (if (set-exists? ans)
            ans
            (fill-world loc))))))

(define get-card-by-xy
  (λ (loc x y)
    (cond
      [(null? loc) #f]
      [else
       (let* ([a (car loc)]
              [d (cdr loc)]
              [p (card-posn a)]
              [p-x (posn-x p)]
              [p-y (posn-y p)]
              [card-x-min (+ (* GAP (add1 p-x)) (* CARD-WIDTH p-x))]
              [card-x-max (+ card-x-min CARD-WIDTH)]
              [card-y-min (+ (* GAP (add1 p-y)) (* CARD-HEIGHT p-y))]
              [card-y-max (+ card-y-min CARD-HEIGHT)])
         (if (and
              (>= x card-x-min)
              (<= x card-x-max)
              (>= y card-y-min)
              (<= y card-y-max))
             a
             (get-card-by-xy d x y)))])))

(define mouse-controls
  (λ (world x y mouse-event)
    (cond
      [(eqv? mouse-event "button-down")
       ]
      [else world])))


(define empty-world '())

(big-bang (fill-world empty-world)
  (to-draw draw-world)
  (on-mouse mouse-controls))
                                                                                                           
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;    ;;;;;;;;;;;;;;;;;;;;       ;;;;;;;;;;;;;;;;            ;;;;;;;          ;;;;;;;;;;;;;;;;;;;;           ;;;;;;;        
;    ;;;;;;;;;;;;;;;;;;;;       ;;;;;;;;;;;;;;;;          ;;;;;;;;;;;        ;;;;;;;;;;;;;;;;;;;;         ;;;;;;;;;;;      
;    ;;;;;;;;;;;;;;;;;;;;       ;;;;;;;;;;;;;;;;         ;;;;;;;;;;;;;;      ;;;;;;;;;;;;;;;;;;;;        ;;;;;;;;;;;;;;    
;            ;;;;               ;;;                     ;;;;       ;;;;              ;;;;               ;;;;       ;;;;    
;            ;;;;               ;;;                    ;;;;          ;;              ;;;;              ;;;;          ;;    
;            ;;;;               ;;;                    ;;;                           ;;;;              ;;;                 
;            ;;;;               ;;;                    ;;;                           ;;;;              ;;;                 
;            ;;;;               ;;;                    ;;;                           ;;;;              ;;;                 
;            ;;;;               ;;;                    ;;;;                          ;;;;              ;;;;                
;            ;;;;               ;;;                     ;;;;                         ;;;;               ;;;;               
;            ;;;;               ;;;                     ;;;;;;;                      ;;;;               ;;;;;;;            
;            ;;;;               ;;;;;;;;;;;;;;           ;;;;;;;;                    ;;;;                ;;;;;;;;          
;            ;;;;               ;;;;;;;;;;;;;;             ;;;;;;;;                  ;;;;                  ;;;;;;;;        
;            ;;;;               ;;;;;;;;;;;;;;               ;;;;;;;;                ;;;;                    ;;;;;;;;      
;            ;;;;               ;;;                             ;;;;;;;              ;;;;                       ;;;;;;;    
;            ;;;;               ;;;                               ;;;;;;             ;;;;                         ;;;;;;   
;            ;;;;               ;;;                                 ;;;;             ;;;;                           ;;;;   
;            ;;;;               ;;;                                  ;;;             ;;;;                            ;;;   
;            ;;;;               ;;;                                  ;;;             ;;;;                            ;;;   
;            ;;;;               ;;;                                  ;;;             ;;;;                            ;;;   
;            ;;;;               ;;;                    ;             ;;;             ;;;;              ;             ;;;   
;            ;;;;               ;;;                   ;;;;          ;;;;             ;;;;             ;;;;          ;;;;   
;            ;;;;               ;;;                   ;;;;;;       ;;;;              ;;;;             ;;;;;;       ;;;;    
;            ;;;;               ;;;;;;;;;;;;;;;;       ;;;;;;;;;;;;;;;               ;;;;              ;;;;;;;;;;;;;;;     
;            ;;;;               ;;;;;;;;;;;;;;;;         ;;;;;;;;;;;;                ;;;;                ;;;;;;;;;;;;      
;            ;;;;               ;;;;;;;;;;;;;;;;            ;;;;;;;                  ;;;;                   ;;;;;;;        
;                                                                                                                          
                                                                                       
(define c1
  (make-card (make-posn 0 0)
        DIAMOND
        ONE
        PATTERN
        GREEN
        #f))
(define c2
  (make-card (make-posn 1 0)
        DIAMOND
        TWO
        PATTERN
        PURPLE
        #f))
(define c3
  (make-card (make-posn 2 0)
        DIAMOND
        THREE
        PATTERN
        RED
        #f))
(define c4
  (make-card (make-posn 0 1)
        DIAMOND
        TWO
        OUTLINE
        PURPLE
        #f))
(define c5
  (make-card (make-posn 1 1)
        DIAMOND
        THREE
        OUTLINE
        RED
        #f))
(define c6
  (make-card (make-posn 2 1)
        SQUIGGLE
        TWO
        PATTERN
        GREEN
        #f))
(define c7
  (make-card (make-posn 0 2)
        DIAMOND
        TWO
        SOLID
        PURPLE
        #f))
(define c8
  (make-card (make-posn 1 2)
        DIAMOND
        THREE
        SOLID
        GREEN
        #f))
(define c9
  (make-card (make-posn 2 2)
        OVAL
        TWO
        OUTLINE
        PURPLE
        #f))
(define c10
  (make-card (make-posn 0 3)
        DIAMOND
        ONE
        SOLID
        RED
        #f))
(define c11
  (make-card (make-posn 1 3)
        DIAMOND
        ONE
        OUTLINE
        PURPLE
        #f))
(define c12
  (make-card (make-posn 2 3)
        DIAMOND
        ONE
        OUTLINE
        GREEN
        #f))

(define lod (list c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12))
;(set-exists? lod)
;(draw-world lod)

(define o1
  (make-card (make-posn 0 0)
        OVAL
        ONE
        SOLID
        RED
        #f))
(define o2
  (make-card (make-posn 1 0)
        DIAMOND
        ONE
        SOLID
        RED
        #f))
(define o4
  (make-card (make-posn 0 1)
        OVAL
        ONE
        OUTLINE
        GREEN
        #f))
(define o5
  (make-card (make-posn 1 1)
        OVAL
        ONE
        PATTERN
        PURPLE
        #f))
(define o7
  (make-card (make-posn 0 2)
        OVAL
        TWO
        PATTERN
        RED
        #f))
(define o8
  (make-card (make-posn 1 2)
        OVAL
        THREE
        PATTERN
        GREEN
        #f))

(define loo (list o1 o2 o4 o5 o7 o8))
;(draw-world loo)

(define s1
  (make-card (make-posn 0 0)
        SQUIGGLE
        ONE
        SOLID
        RED
        #f))
(define s2
  (make-card (make-posn 1 0)
        DIAMOND
        ONE
        SOLID
        RED
        #f))
(define s4
  (make-card (make-posn 0 1)
        SQUIGGLE
        ONE
        OUTLINE
        GREEN
        #f))
(define s5
  (make-card (make-posn 1 1)
        SQUIGGLE
        ONE
        PATTERN
        PURPLE
        #f))
(define s7
  (make-card (make-posn 0 2)
        SQUIGGLE
        TWO
        PATTERN
        RED
        #f))
(define s8
  (make-card (make-posn 1 2)
        SQUIGGLE
        THREE
        PATTERN
        GREEN
        #f))
(define s9
  (make-card (make-posn 0 3)
        SQUIGGLE
        TWO
        OUTLINE
        PURPLE
        #f))
(define s10
  (make-card (make-posn 1 3)
        SQUIGGLE
        THREE
        SOLID
        PURPLE
        #f))

(define los (list s1 s2 s4 s5 s7 s8 s9 s10))
;(draw-world los)