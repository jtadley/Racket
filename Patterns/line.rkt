#lang racket

(require
  2htdp/image
  2htdp/universe
  racket/struct)

(define WIDTH 800)
(define HEIGHT 800)
(define PEN (make-pen "cyan" 3 "solid" "round" "miter"))
(define BG_COLOR "black")
(define π pi)
(define max_diff (/ π 16))
(define step_size (/ 1 4))
(define tick-speed 0.01)

(struct head
  (x
   y
   dir)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (h) 'head)
      (λ (h) (list (head-x h)
                   (head-y h)
                   (head-dir h)))))])

(define init_world (list (head (/ WIDTH 2) (/ HEIGHT 2) (/ (* 3 π) 2))))

(define draw_world
  (λ (world)
    (cond
      [(or (null? world)
           (null? (cdr world)))
       (rectangle WIDTH HEIGHT "solid" BG_COLOR)]
      [else (add-line
             (draw_world (cdr world))
             (head-x (car world))
             (head-y (car world))
             (head-x (car (cdr world)))
             (head-y (car (cdr world)))
             PEN)])))

(define next_head
  (λ (h)
    (let* ([h_x (head-x h)]
           [h_y (head-y h)]
           [old_dir (head-dir h)]
           [h_dir (cond
                    [(or (< h_x 0)
                         (>= h_x WIDTH))
                     (atan (* -1 (cos old_dir)) (sin old_dir))]
                    [(or (< h_y 0)
                         (>= h_y HEIGHT))
                     (atan (cos old_dir) (* -1 (sin old_dir)))]
                    [else old_dir])])
      (let* ([sign (if (> (random) 0.5) 1 -1)]
             [dir_change_unsigned (* (random) max_diff)]
             [dir_change (* sign dir_change_unsigned)]
             [new_dir (+ h_dir dir_change)]
             [new_x (+ h_x (* step_size (cos new_dir)))]
             [new_y (+ h_y (* step_size) (sin new_dir))])
        (head new_x new_y new_dir)))))

(define tick_controls
  (λ (world)
    (cons (next_head (car world)) world)))


(big-bang init_world
  (to-draw draw_world)
  (on-tick tick_controls tick-speed))