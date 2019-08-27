#lang racket

(require
  2htdp/image
  2htdp/universe
  rackunit)


(define SCREEN_WIDTH 1000)
(define SCREEN_HEIGHT 1000)
(define LINE_LENGTH 20)
(define BG_COLOR "white")
(define PEN (make-pen "black" 2 "solid" "round" "miter"))

(define isTwoPlus3k?
  (λ (n)
    (eqv? (modulo (/ n LINE_LENGTH) 3)
          2)))

(define draw_board
  (λ (x y)
    (cond
      [(>= y SCREEN_HEIGHT) (rectangle SCREEN_WIDTH SCREEN_HEIGHT "solid" BG_COLOR)]
      [(>= x SCREEN_WIDTH) (draw_board 0 (+ y (* 2 LINE_LENGTH)))]
      ;; draw a box
      [(isTwoPlus3k? x)
       (scene+line
        (scene+line
         (scene+line
          (scene+line
           (draw_board (+ x LINE_LENGTH) y)
           ;; top
           x
           (+ y (/ LINE_LENGTH 2))
           (+ x LINE_LENGTH)
           (+ y (/ LINE_LENGTH 2))
           PEN)
          ;; left
          x
          (+ y (/ LINE_LENGTH 2))
          x
          (+ y (* (/ LINE_LENGTH 2) 3))
          PEN)
         ;; bottom
         x
         (+ y (* (/ LINE_LENGTH 2) 3)) 
         (+ x LINE_LENGTH)
         (+ y (* (/ LINE_LENGTH 2) 3))
         PEN)
        ;; right
        (+ x LINE_LENGTH)
        (+ y (/ LINE_LENGTH 2))
        (+ x LINE_LENGTH)
        (+ y (* (/ LINE_LENGTH 2) 3))
        PEN)]
      ;; draw a hexagon
      [else
       (scene+line
        (scene+line
         (scene+line
          (scene+line
           (scene+line
            (scene+line
             (scene+line
              (scene+line
               (draw_board (+ x (* 2 LINE_LENGTH)) y)
               ;; top
               (+ x (/ LINE_LENGTH 2))
               y
               (+ x (* (/ LINE_LENGTH 2) 3))
               y
               PEN)
              ;; top-right
              (+ x (* (/ LINE_LENGTH 2) 3))
              y
              (+ x (* 2 LINE_LENGTH))
              (+ y (/ LINE_LENGTH 2))
              PEN)
             ;; right
             (+ x (* 2 LINE_LENGTH))
             (+ y (/ LINE_LENGTH 2))
             (+ x (* 2 LINE_LENGTH))
             (+ y (* (/ LINE_LENGTH 2) 3))
             PEN)
            ;; bottom-right
            (+ x (* 2 LINE_LENGTH))
            (+ y (* (/ LINE_LENGTH 2) 3))
            (+ x (* (/ LINE_LENGTH 2) 3))
            (+ y (* 2 LINE_LENGTH))
            PEN)
           ;; top-left
           (+ x (/ LINE_LENGTH 2))
           y
           x
           (+ y (/ LINE_LENGTH 2))
           PEN)
          ;; left
          x
          (+ y (/ LINE_LENGTH 2))
          x
          (+ y (* (/ LINE_LENGTH 2) 3))
          PEN)
         ;; bottom-left
         x
         (+ y (* (/ LINE_LENGTH 2) 3))
         (+ x (/ LINE_LENGTH 2))
         (+ y (* 2 LINE_LENGTH))
         PEN)
        ;; bottom
        (+ x (/ LINE_LENGTH 2))
        (+ y (* 2 LINE_LENGTH))
        (+ x (* (/ LINE_LENGTH 2) 3))
        (+ y (* 2 LINE_LENGTH))
        PEN)])))

;(draw_board 0 0)

(struct posn (x y))
;; state is one of: "active", "dormant", "flagged"
;; type is one of: "square", "hexagon", "octagon"
(struct block (x y type state neighbour-posns))
(define block-init
  (λ (x y type)
     (block x y type "dormant"
            (match type
                   ["square" (list (posn ) ;; left-octagon
                                   (posn ) ;; right-octagon
                                   (posn ) ;; top-hexagon
                                   (posn ) ;; bottom-hexagon
                                   )]
                   ["hexagon" (list (posn ) ;; top-left-octagon
                                    (posn ) ;; top-right-octagon
                                    (posn ) ;; bottom-left-octagon
                                    (posn ) ;; bottom-right-octagon
                                    (posn ) ;; top-square
                                    (posn ) ;; bottom-square
                                    )]
                   ["octagon" (list (posn ) ;; top-octagon
                                    (posn ) ;; bottom-octagon
                                    (posn ) ;; top-left-hexagon
                                    (posn ) ;; top-right-hexagon
                                    (posn ) ;; bottom-left-hexagon
                                    (posn ) ;; bottom-right-hexagon
                                    (posn ) ;; left-square
                                    (posn ) ;; right-square
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
              (if (<= (+ y LINE_LENTGTH) SCREEN_HEIGHT)
                  (cons (block-init x (+ y (* (/ LINE_LENGTH 2) 3)) "hexagon")
                        (gen-init (+ x LINE_LENGTH) y))
                  (gen-init (+ x LINE_LENGTH) y)))]
       ;; add an octagon
       [else (cons (block-init (+ x (/ LINE_LENGTH 2)) y "octagon")
                   (gen-init (+ x (* 2 LINE_LENGTH) y)))])))

(define init (gen-init 0 0))
