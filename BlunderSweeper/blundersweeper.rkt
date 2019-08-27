#lang racket

(require
  2htdp/image
  2htdp/universe
  rackunit
  lang/posn)


(define SCREEN_WIDTH 1000)
(define SCREEN_HEIGHT 1000)
(define LINE_LENGTH 20)
(define BG_COLOR "white")
(define PEN (make-pen "black" 2 "solid" "round" "miter"))
(define POLYGON_BG_COLOR "white")
(define POLYGON_LINE_COLOR "black")
(define MINE_PROBABILITY .25)

(define isTwoPlus3k?
  (λ (n)
    (eqv? (modulo (/ n LINE_LENGTH) 3)
          2)))

(struct posn (x y))

;; state is one of: "active", "dormant", "flagged"
;; type is one of: "square", "hexagon", "octagon"
(struct block (x y type state mine? neighbour_posns))

(define block-init
  (λ (x y type)
    (block x y type "dormant" (< (random) MINE_PROBABILITY)
           (match type
             ["square" (list (posn (- x (* (/ LINE_LENGTH 2) 3))
                                   (- y (/ LINE_LENGTH 2))) ;; left-octagon
                             (posn (+ x (* (/ LINE_LENGTH 2) 3))
                                   (- y (/ LINE_LENGTH 2))) ;; right-octagon
                             (posn x
                                   (- y LINE_LENGTH)) ;; top-hexagon
                             (posn x
                                   (+ y LINE_LENGTH)) ;; bottom-hexagon
                             )]
             ["hexagon" (list (posn (- x (* (/ LINE_LENGTH 2) 3))
                                    (- y (* (/ LINE_LENGTH 2) 3))) ;; top-left-octagon
                              (posn (+ x (* (/ LINE_LENGTH 2) 3))
                                    (- y (* (/ LINE_LENGTH 2) 3))) ;; top-right-octagon
                              (posn (- x (* (/ LINE_LENGTH 2) 3))
                                    (+ y (/ LINE_LENGTH 2))) ;; bottom-left-octagon
                              (posn (+ x (* (/ LINE_LENGTH 2) 3))
                                    (+ y (/ LINE_LENGTH 2))) ;; bottom-right-octagon
                              (posn x
                                    (- y LINE_LENGTH)) ;; top-square
                              (posn x
                                    (+ y LINE_LENGTH)) ;; bottom-square
                              )]
             ["octagon" (list (posn x
                                    (- y (* 2 LINE_LENGTH))) ;; top-octagon
                              (posn x
                                    (+ y (* 2 LINE_LENGTH))) ;; bottom-octagon
                              (posn (- x (* (/ LINE_LENGTH 2) 3))
                                    (- y (/ LINE_LENGTH 2))) ;; top-left-hexagon
                              (posn (+ x (* (/ LINE_LENGTH 2) 3))
                                    (- y (/ LINE_LENGTH 2))) ;; top-right-hexagon
                              (posn (- x (* (/ LINE_LENGTH 2) 3))
                                    (+ y (* (/ LINE_LENGTH 2) 3))) ;; bottom-left-hexagon
                              (posn (+ x (* (/ LINE_LENGTH 2) 3))
                                    (+ y (* (/ LINE_LENGTH 2) 3))) ;; bottom-right-hexagon
                              (posn (- x (* (/ LINE_LENGTH 2) 3))
                                    (+ y (/ LINE_LENGTH 2))) ;; left-square
                              (posn (+ x (* (/ LINE_LENGTH 2) 3))
                                    (+ y (/ LINE_LENGTH 2))) ;; right-square
                              )]))))

;; returns a list of blocks that is the game state
(define gen-init
  (λ (x y)
    (cond
      [(>= y SCREEN_HEIGHT) '()]
      [(>= x SCREEN_WIDTH) (gen-init 0 (+ y (* 2 LINE_LENGTH)))]
      ;; add a box and a hexagon
      [(isTwoPlus3k? x)
       (cons (block-init x (+ y (/ LINE_LENGTH 2)) "square")
             (if (<= (+ y LINE_LENGTH) SCREEN_HEIGHT)
                 (cons (block-init x (+ y (* (/ LINE_LENGTH 2) 3)) "hexagon")
                       (gen-init (+ x LINE_LENGTH) y))
                 (gen-init (+ x LINE_LENGTH) y)))]
      ;; add an octagon
      [else (cons (block-init (+ x (/ LINE_LENGTH 2)) y "octagon")
                  (gen-init (+ x (* 2 LINE_LENGTH)) y))])))

(define init (gen-init 0 0))

(define draw-world
  (λ (lob)
    (if (null? lob)
        (rectangle SCREEN_WIDTH SCREEN_HEIGHT "solid" BG_COLOR)
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
          (scene+polygon
           (scene+polygon
            (draw-world (cdr lob))
            lop
            "solid"
            POLYGON_BG_COLOR)
           lop
           "outline"
           POLYGON_LINE_COLOR)))))

(define get-block
  (λ (x y world)
    (cond
      [(null? world) (error 'block-not-found)]
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
                               [b-value (if (block-mine? b) 1 0)])
                          (+ b-value (helper (cdr lop))))]))])
      (helper (block-neighbour_posns b)))))

(draw-world init)
