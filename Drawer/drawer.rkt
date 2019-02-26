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
(define line_color (color 0 0 0))
(define rainbow? #t)
(define diff_n 0) ; should be set to number of init points - 1
(define progression_factor 0.05)
(define randomness_factor 0) ; integer [0,inf)
(define over_edge? #f)
(define tick-speed 0.01)
(define side_len 5)
(define start_posn (posn (/ WIDTH 2) (/ HEIGHT 2)))

(define poly_points
  (λ (n)
    (letrec ([dst (/ (* 2 pi) n)]
             [helper
              (λ (n)
                (cond
                  [(zero? n) '()]
                  [else (let ([angle (* (sub1 n) dst)])
                            (cons
                             (posn (+ (* (cos angle) side_len) (posn-x start_posn))
                                   (+ (* (sin angle) side_len) (posn-y start_posn)))
                             (helper (sub1 n))))]))])
      (helper n))))

(define mutate-line-color
  (λ ()
    (let ([r (color-red line_color)]
          [g (color-green line_color)]
          [b (color-blue line_color)])
      (if (equal? r 125)
          (if (equal? g 125)
              (if (equal? b 125)
                  (set! line_color
                        (color 0 0 0))
                  (set! line_color
                        (color r g (+ b 5))))
              (set! line_color
                    (color r (+ g 5) b)))
          (set! line_color
                (color (+ r 5) g b))))))

(define draw_world
  (λ (world)
    (cond
      [(or (null? world)
           (null? (cdr world)))
       (empty-scene WIDTH HEIGHT)]
      [else (if rainbow?
                (mutate-line-color)
                (void))
            (add-line
             (draw_world (cdr world))
             (posn-x (car world))
             (posn-y (car world))
             (posn-x (car (cdr world)))
             (posn-y (car (cdr world)))
             (color (color-red line_color)
                    (color-green line_color)
                    (color-blue line_color)))])))

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
              [new_x (+
                      (+ (* d_x progression_factor) p_along_x)
                      (* randomness_factor (random)))]
              [new_y (+
                      (+ (* d_y progression_factor) p_along_y)
                      (* randomness_factor (random)))])
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

(define init_world_3
  (list
   (posn 398 402)
   (posn 400 398)
   (posn 402 402)))

(define init_world_5
  (list
   (posn 398 400)
   (posn 398 400)
   (posn 399 402)
   (posn 401 402)
   (posn 402 400)))

(define go_fake_3
  (λ ()
    (set! over_edge? #f)
    (set! diff_n 2)
    (go init_world_3)))

(define go_fake_5
  (λ ()
    (set! over_edge? #f)
    (set! diff_n 4)
    (go init_world_5)))

(define go_n
  (λ (n)
    (set! over_edge? #f)
    (set! diff_n (sub1 n))
    (go (poly_points n))))

(define go
  (λ (init_world)
    (set! over_edge? #f)
    (big-bang init_world
      (to-draw draw_world)
      (on-tick tick_controls tick-speed))))