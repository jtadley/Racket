#lang racket

(require 2htdp/image)

(define WIDTH 800)
(define HEIGHT 800)
(define X_INCR 20)
(define Y_INCR (* X_INCR (/ 3 4)))
(define PEN (make-pen "cyan" 2 "solid" "projecting" "bevel"))
(define BG_COLOR "black")

(define draw_intersection
  (λ (x y)
    (scene+line
     (scene+line
      (scene+line
       (scene+line
        (scene+line
         (draw_squares (add1 x) y)
         x
         y
         (+ x X_INCR)
         (- y Y_INCR)
         PEN)
        x
        y
        (+ x X_INCR)
        (+ y Y_INCR)
        PEN)
       x
       y
       (- x X_INCR)
       (- y Y_INCR)
       PEN)
      x
      y
      (- x X_INCR)
      (+ y Y_INCR)
      PEN)
     x
     (- y Y_INCR)
     x
     (+ y Y_INCR)
     PEN)))

(define draw_squares
  (λ (x y)
    (cond
      [(>= y HEIGHT) (rectangle (- WIDTH X_INCR) (- HEIGHT Y_INCR) "solid" BG_COLOR)]
      [(>= x WIDTH) (draw_squares 0 (add1 y))]
      [(and (zero? (modulo x (* 2 X_INCR)))
            (eqv? (modulo y (* 4 Y_INCR)) Y_INCR))
       (draw_intersection x y)]
      [(and (eqv? (modulo x (* 2 X_INCR)) X_INCR)
            (eqv? (modulo y (* 4 Y_INCR)) (* 3 Y_INCR)))
       (draw_intersection x y)]
      [else (draw_squares (add1 x) y)])))

(define draw
  (λ ()
    (draw_squares 0 0)))

(draw)