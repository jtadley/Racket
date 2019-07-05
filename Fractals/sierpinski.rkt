#lang racket

(require
  2htdp/image
  2htdp/universe
  lang/posn)

(define WIDTH 800)
(define HEIGHT 800)
(define PEN (make-pen "cyan" 2 "solid" "projecting" "bevel"))
(define BG_COLOR "black")
(define DEPTH 7)
(define tick-speed 0.5)

(struct posn (x y))
(struct triangle (p1 p2 p3))
(struct world (new old))

(define init_world (world (list (triangle
                                 (posn (/ WIDTH 2) 0)
                                 (posn 0 (- HEIGHT 1))
                                 (posn (- WIDTH 1) (- HEIGHT 1))))
                          '()))

(define draw_world
  (λ (w)
    (letrec ([helper
              (λ (ls)
                (cond
                  [(null? ls) (rectangle WIDTH HEIGHT "solid" BG_COLOR)]
                  [else 
                   (let ([t (car ls)])
                     (scene+polygon
                      (helper (cdr ls))
                      (list (make-posn (posn-x (triangle-p1 t)) (posn-y (triangle-p1 t)))
                            (make-posn (posn-x (triangle-p2 t)) (posn-y (triangle-p2 t)))
                            (make-posn (posn-x (triangle-p3 t)) (posn-y (triangle-p3 t))))
                      "outline"
                      PEN))]))])
      (helper (world-old w)))))

(define midpoint
  (λ (p1 p2)
    (posn
     (/ (+ (posn-x p1) (posn-x p2)) 2)
     (/ (+ (posn-y p1) (posn-y p2)) 2))))

(define one_step_triangle
  (λ (t)
    (let* ([t_p1 (triangle-p1 t)]
           [t_p2 (triangle-p2 t)]
           [t_p3 (triangle-p3 t)]
           [p1_p2_midpt (midpoint t_p1 t_p2)]
           [p2_p3_midpt (midpoint t_p2 t_p3)]
           [p3_p1_midpt (midpoint t_p3 t_p1)]
           [t1 (triangle
                t_p1
                p1_p2_midpt
                p3_p1_midpt)]
           [t2 (triangle
                p1_p2_midpt
                t_p2
                p2_p3_midpt)]
           [t3 (triangle
                p3_p1_midpt
                p2_p3_midpt
                t_p3)])
      (list t1 t2 t3))))

(define next_world
  (λ (w)
    (if (>= (length (world-old w)) (expt 3 (sub1 DEPTH)))
        w
        (world
         (foldr
          (λ (t almost)
            (append (one_step_triangle t) almost))
          '()
          (world-new w))
         (world-new w)))))

(big-bang init_world
  (to-draw draw_world)
  (on-tick next_world tick-speed))
