#lang racket

(require
  2htdp/image
  2htdp/universe
  racket/struct
  rackunit)

(struct posn
  (x
   y)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (p) 'posn)
      (λ (p) (list (posn-x p)
                   (posn-y p)))))])

(define WIDTH 800)
(define HEIGHT 800)
(define line_color "black")
(define diff_n 2) ; should be set to number of init points - 1
(define progress_factor 0.1)
(define over_edge? #f)

(define draw_world
  (λ (world)
    (cond
      [(or (null? world)
           (null? (cdr world)))
       (empty-scene WIDTH HEIGHT)]
      [else (add-line
             (draw_world (cdr world))
             (posn-x (car world))
             (posn-y (car world))
             (posn-x (car (cdr world)))
             (posn-y (car (cdr world)))
             line_color)])))

(define cdr_n
  (λ (ls n)
    (cond
      [(equal? n 0) (car ls)]
      [else (cdr_n (cdr ls) (sub1 n))])))

(define get_next_posn
  (λ (world)
    (cond
      [(< (length world) (add1 diff_n)) (error "badness (add1 9k)")]
      [else
       (let* ([p_start (car world)]              
              [p_start_x (posn-x p_start)]
              [p_start_y (posn-y p_start)]
              [p_along (cdr_n world diff_n)]
              [p_along_x (posn-x p_along)]
              [p_along_y (posn-y p_along)]
              [d_x (- p_along_x p_start_x)]
              [d_y (- p_along_y p_start_y)]
              [new_x (+ (* d_x progress_factor) p_along_x)]
              [new_y (+ (* d_y progress_factor) p_along_y)])
         (posn new_x new_y))])))

(define is_over_edge?
  (λ (p)
    (or
     (>= (posn-x p) WIDTH)
     (< (posn-x p) 0)
     (>= (posn-y p) HEIGHT)
     (< (posn-y p) 0))))

(define tick_controls
  (λ (world)
    (if (not over_edge?)
        (let ([next_posn (get_next_posn world)])
          (if (is_over_edge? next_posn)
              (begin
                (set! over_edge? #t)
                world)
              (cons
               next_posn
               world)))
        world)))

(define init_world_3 (list (posn 398 402) (posn 400 398) (posn 402 402)))
(define init_world_5 (list (posn 398 400) (posn 398 400) (posn 399 402) (posn 401 402) (posn 402 400)))

(big-bang init_world_3
          (to-draw draw_world)
          (on-tick tick_controls 0.05))