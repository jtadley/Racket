#lang racket

(require 2htdp/image)
(require lang/posn)

(define WIDTH 800)
(define HEIGHT 800)
(define SIZE 20)
(define OFFSET (* 10 SIZE))
(define BG_COLOR "black")
(define PEN (make-pen "cyan" (/ SIZE 5) "solid" "round" "miter"))

(define tmp (scene+polygon
             (empty-scene WIDTH HEIGHT)
             (list (make-posn 49 49)
                   (make-posn (+ 49 2 (* 10 SIZE)) 49)
                   (make-posn (+ 49 2 (* 10 SIZE)) (+ 49 2 (* 10 SIZE)))
                   (make-posn 49 (+ 49 2 (* 10 SIZE))))
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
           (+ x (* 6 SIZE))
           y
           PEN)
          x
          (+ y (* 8 SIZE))
          (+ x (* 6 SIZE))
          (+ y (* 8 SIZE))
          PEN)
         (+ x (* 3 SIZE))
         y
         (+ x (* 3 SIZE))
         (+ y (* 8 SIZE))
         PEN)
        (+ x SIZE)
        y
        (+ x SIZE)
        (+ y SIZE)
        PEN)
       (+ x (* 5 SIZE))
       y
       (+ x (* 5 SIZE))
       (+ y SIZE)
       PEN)
      (+ x SIZE)
      (+ y (* 7 SIZE))
      (+ x SIZE)
      (+ y (* 8 SIZE))
      PEN)
     (+ x (* 5 SIZE))
     (+ y (* 7 SIZE))
     (+ x (* 5 SIZE))
     (+ y (* 8 SIZE))
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
           (+ x (* 6 SIZE))
           PEN)
          (+ y (* 8 SIZE))
          x
          (+ y (* 8 SIZE))
          (+ x (* 6 SIZE))
          PEN)
         y
         (+ x (* 3 SIZE))
         (+ y (* 8 SIZE))
         (+ x (* 3 SIZE))
         PEN)
        y
        (+ x SIZE)
        (+ y SIZE)
        (+ x SIZE)
        PEN)
       y
       (+ x (* 5 SIZE))
       (+ y SIZE)
       (+ x (* 5 SIZE))
       PEN)
      (+ y (* 7 SIZE))
      (+ x SIZE)
      (+ y (* 8 SIZE))
      (+ x SIZE)
      PEN)
     (+ y (* 7 SIZE))
     (+ x (* 5 SIZE))
     (+ y (* 8 SIZE))
     (+ x (* 5 SIZE))
     PEN)))

(define draw_maze
  (λ (x y)
    (cond
      [(>= y (+ HEIGHT OFFSET))
       (rectangle WIDTH HEIGHT "solid" BG_COLOR)]
      [(>= x (+ WIDTH OFFSET))
       (draw_maze (- 0 OFFSET) (add1 y))]
      [(or
        (and
         (zero? (modulo x (* 10 SIZE)))
         (zero? (modulo y (* 10 SIZE))))
        (and
         (eqv? (modulo x (* 10 SIZE)) (* 5 SIZE))
         (eqv? (modulo y (* 10 SIZE)) (* 5 SIZE))))
       (draw_block_vertical x y (draw_maze (add1 x) y))]
      [(or
        (and
         (eqv? (modulo x (* 10 SIZE)) (* 4 SIZE))
         (eqv? (modulo y (* 10 SIZE)) SIZE))
        (and
         (eqv? (modulo x (* 10 SIZE)) (* 9 SIZE))
         (eqv? (modulo y (* 10 SIZE)) (* 6 SIZE))))
       (draw_block_horizontal y x (draw_maze (add1 x) y))]
      [else (draw_maze (add1 x) y)])))

(draw_maze (- 0 OFFSET) (- 0 OFFSET))
