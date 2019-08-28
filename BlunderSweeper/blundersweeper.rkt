#lang racket

(require
  2htdp/image
  2htdp/universe
  rackunit
  lang/posn
  racket/struct)


(define SCREEN_WIDTH 1000)
(define SCREEN_HEIGHT 1000)
(define LINE_LENGTH 20)
(define BG_COLOR "white")
(define PEN (make-pen "black" 2 "solid" "round" "miter"))
(define POLYGON_BG_COLOR "white")
(define POLYGON_LINE_COLOR "black")
(define MINE_PROBABILITY .25)

(define LEN-3/2 (* (/ LINE_LENGTH 2) 3))
(define LEN-1/2 (/ LINE_LENGTH 2))
(define LEN-2 (* LINE_LENGTH 2))

(define isTwoPlus3k?
  (λ (n)
    (eqv? (modulo (/ n LINE_LENGTH) 3)
          2)))

(struct posn (x y)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (p) 'posn)
      (λ (p) (list (posn-x p) (posn-y p)))))])

(define ACTIVE "active")
(define DORMANT "dormant")
(define FLAGGED "flagged")
(define SQUARE "square")
(define HEXAGON "hexagon")
(define OCTAGON "octagon")
;; state is one of: "active", "dormant", "flagged"
;; type is one of: "square", "hexagon", "octagon"
(struct block
  (x
   y
   type
   state
   mine?
   number
   neighbour_posns)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (b) 'block)
      (λ (b) (list
              (block-x b)
              (block-y b)
              (block-type b)
              (block-mine? b)
              (block-neighbour_posns b)))))])

(define edit-number
  (λ (b world)
    (block
     (block-x b)
     (block-y b)
     (block-type b)
     (block-state b)
     (block-mine? b)
     (get-block-number b world)
     (block-neighbour_posns b))))

(define edit-state
  (λ (b state)
    (block
     (block-x b)
     (block-y b)
     (block-type b)
     state
     (block-mine? b)
     (block-number b)
     (block-neighbour_posns b))))

(define block-init
  (λ (x y type)
    (block x y type DORMANT (< (random) MINE_PROBABILITY) 0
           (match type
             ["square" (list (posn (- x LEN-3/2)
                                   (- y LEN-1/2)) ;; left-octagon
                             (posn (+ x LEN-3/2)
                                   (- y LEN-1/2)) ;; right-octagon
                             (posn x
                                   (- y LINE_LENGTH)) ;; top-hexagon
                             (posn x
                                   (+ y LINE_LENGTH)) ;; bottom-hexagon
                             )]
             ["hexagon" (list (posn (- x LEN-3/2)
                                    (- y LEN-3/2)) ;; top-left-octagon
                              (posn (+ x LEN-3/2)
                                    (- y LEN-3/2)) ;; top-right-octagon
                              (posn (- x LEN-3/2)
                                    (+ y LEN-1/2)) ;; bottom-left-octagon
                              (posn (+ x LEN-3/2)
                                    (+ y LEN-1/2)) ;; bottom-right-octagon
                              (posn x
                                    (- y LINE_LENGTH)) ;; top-square
                              (posn x
                                    (+ y LINE_LENGTH)) ;; bottom-square
                              )]
             ["octagon" (list (posn x
                                    (- y LEN-2)) ;; top-octagon
                              (posn x
                                    (+ y LEN-2)) ;; bottom-octagon
                              (posn (- x LEN-3/2)
                                    (- y LEN-1/2)) ;; top-left-hexagon
                              (posn (+ x LEN-3/2)
                                    (- y LEN-1/2)) ;; top-right-hexagon
                              (posn (- x LEN-3/2)
                                    (+ y LEN-3/2)) ;; bottom-left-hexagon
                              (posn (+ x LEN-3/2)
                                    (+ y LEN-3/2)) ;; bottom-right-hexagon
                              (posn (- x LEN-3/2)
                                    (+ y LEN-1/2)) ;; left-square
                              (posn (+ x LEN-3/2)
                                    (+ y LEN-1/2)) ;; right-square
                              )]))))

;; returns a list of blocks that is the game state
(define gen-init
  (λ (x y)
    (cond
      [(>= y (- SCREEN_HEIGHT LINE_LENGTH)) '()]
      [(>= x SCREEN_WIDTH) (gen-init 0 (+ y LEN-2))]
      ;; add a box and a (maybe) hexagon
      [(isTwoPlus3k? x)
       (cons (block-init x (+ y LEN-1/2) SQUARE)
             (if (< y (- SCREEN_HEIGHT LEN-2))
                 (cons (block-init x (+ y LEN-3/2) HEXAGON)
                       (gen-init (+ x LINE_LENGTH) y))
                 (gen-init (+ x LINE_LENGTH) y)))]
      ;; add an octagon
      [else (cons (block-init (+ x LEN-1/2) y OCTAGON)
                  (gen-init (+ x LEN-2) y))])))

(define draw-world
  (λ (lob)
    (if (null? lob)
        (rectangle (add1 SCREEN_WIDTH) (add1 SCREEN_HEIGHT) "solid" BG_COLOR)
        (let* ([b (car lob)]
               [x (block-x b)]
               [y (block-y b)]
               [lop (match (block-type b)
                      ["square"
                       (list 
                        (make-posn x
                                   y)
                        (make-posn x
                                   (+ y LINE_LENGTH))
                        (make-posn (+ x LINE_LENGTH)
                                   (+ y LINE_LENGTH))
                        (make-posn (+ x LINE_LENGTH)
                                   y))]
                      ["hexagon"
                       (list
                        (make-posn x
                                   y)
                        (make-posn (+ x LINE_LENGTH)
                                   y)
                        (make-posn (+ x (* (/ LINE_LENGTH 2) 3))
                                   (+ y (/ LINE_LENGTH 2)))
                        (make-posn (+ x LINE_LENGTH)
                                   (+ y LINE_LENGTH))
                        (make-posn x
                                   (+ y LINE_LENGTH))
                        (make-posn (- x (/ LINE_LENGTH 2))
                                   (+ y (/ LINE_LENGTH 2))))]
                      ["octagon"
                       (list
                        (make-posn x
                                   y)
                        (make-posn (+ x LINE_LENGTH)
                                   y)
                        (make-posn (+ x (* (/ LINE_LENGTH 2) 3))
                                   (+ y (/ LINE_LENGTH 2)))
                        (make-posn (+ x (* (/ LINE_LENGTH 2) 3))
                                   (+ y (* (/ LINE_LENGTH 2) 3)))
                        (make-posn (+ x LINE_LENGTH)
                                   (+ y (* 2 LINE_LENGTH)))
                        (make-posn x
                                   (+ y (* 2 LINE_LENGTH)))
                        (make-posn (- x (/ LINE_LENGTH 2))
                                   (+ y (* (/ LINE_LENGTH 2) 3)))
                        (make-posn (- x (/ LINE_LENGTH 2))
                                   (+ y (/ LINE_LENGTH 2))))])])
          (place-image
           (cond
             [(eqv? (block-state b) DORMANT) (text "" 1 "black")]
             [(block-mine? b) (text "💣" (/ LINE_LENGTH 2) "black")]
             [else (text (number->string (block-number b)) (/ LINE_LENGTH 2) "maroon")])
           (+ x (/ LINE_LENGTH 2))
           (+ y (/ LINE_LENGTH 2))
           (scene+polygon
            (scene+polygon
             (draw-world (cdr lob))
             lop
             "solid"
             POLYGON_BG_COLOR)
            lop
            "outline"
            POLYGON_LINE_COLOR))))))

(define get-block
  (λ (x y world)
    (cond
      [(null? world) #f]
      [(and
        (eqv? x (block-x (car world)))
        (eqv? y (block-y (car world))))
       (car world)]
      [else (get-block x y (cdr world))])))

(define get-block-number
  (λ (b world)
    (letrec ([helper
              (λ (lop)
                (cond
                  [(null? lop) 0]
                  [else (let* ([p (car lop)]
                               [x (posn-x p)]
                               [y (posn-y p)]
                               [b (get-block x y world)]
                               [b-value (if (and b (block-mine? b)) 1 0)])
                          (+ b-value (helper (cdr lop))))]))])
      (helper (block-neighbour_posns b)))))

(define activate-all
  (λ (world)
    (cond
      [(null? world) '()]
      [else (cons (edit-state (car world) ACTIVE)
                  (activate-all (cdr world)))])))

(define xy-in-block?
  (λ (x y b)
    (let ([b-x (block-x b)]
          [b-y (block-y b)])
        (match (block-type b)
          [SQUARE
           (and
            (> x b-x)
            (< x (+ b-x LINE_LENGTH))
            (> y b-y)
            (< y (+ b-y LINE_LENGTH)))]
          [HEXAGON #f]
          [OCTAGON #f]))))

(define activate-block
  (λ (x y world)
    (cond
      [(null? world) '()]
      [(xy-in-block? x y (car world))
       (cons
        (edit-state (car world) ACTIVE)
        (cdr world))]
      [else (cons (car world)
                  (activate-block x y (cdr world)))])))

(define mouse-controls
  (λ (world x y mouse-event)
    (cond
      [(eqv? mouse-event "button-down")
       (printf mouse-event)
       (activate-all world)
       #;
       (activate-block x y world)]
      [else world])))

(define fill-numbers
  (λ (lob world)
    (cond
      [(null? lob) '()]
      [else (cons (edit-number (car lob) world)
                  (fill-numbers (cdr lob) world))])))

(define init
  (let ([init-no-numbers (gen-init 0 0)])
    (fill-numbers init-no-numbers init-no-numbers)))


(big-bang init
  (to-draw draw-world)
  (on-mouse mouse-controls))
