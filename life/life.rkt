#lang racket/base

;; Jacob Adley

(require
  2htdp/batch-io
  2htdp/image
  2htdp/universe
  rackunit)

(define fName "life_init1") ;live_init3

(define SCREEN_WIDTH 1000)

(define SCREEN_HEIGHT 1000)

(define DIAMETER 10)

(define RADIUS (/ DIAMETER 2))

(define NUM_COL (quotient SCREEN_WIDTH DIAMETER))

(define NUM_ROW (quotient SCREEN_HEIGHT DIAMETER))

(struct posn (x y))

;(my_random (current-inexact-milliseconds))
(define my_random
  (λ (time_ms)
    (let ([time (number->string time_ms)])
      (even? (string->number (substring time 0 (- (string-length time) 3)))))))

(define init_live_posns
  (λ (file)
    (letrec ([helper
              (λ (los)
                (cond
                  [(null? los) '()]
                  [else (cons (posn (string->number (car los))
                                    (string->number (car (cdr los))))
                              (helper (cdr (cdr los))))]))])
      (helper (read-words file)))))

(define random_init
  (letrec ([helper
            (λ (col row)
              (cond
                  [(> row NUM_ROW) '()]
                  [(> col NUM_COL) (helper (add1 col) row)]
                  [else (if (my_random (current-inexact-milliseconds)) ;(equal? (random 2) 1)
                            (cons (posn col row)
                                  (helper row (add1 col)))
                            (helper row (add1 col)))]))])
    (helper 0 0)))

(define draw_world
  (λ (world)
    (cond
      [(null? world) (empty-scene SCREEN_WIDTH SCREEN_HEIGHT)]
      [else (place-image
             (circle RADIUS
                     "solid"
                     "black")
             (* (posn-x (car world)) DIAMETER)
             (* (posn-y (car world)) DIAMETER)
             (draw_world (cdr world)))])))

(define mem
  (λ (p world)
    (cond
      [(null? world) #f]
      [(and (equal? (posn-x p) (posn-x (car world)))
            (equal? (posn-y p) (posn-y (car world))))
       #t]
      [else (mem p (cdr world))])))

(define count_live_neighbours
  (λ (world)
    (λ (col row)
      (+
       (if (mem (posn (sub1 col) (sub1 row)) world)
           1
           0)
       (if (mem (posn (sub1 col) row) world)
           1
           0)
       (if (mem (posn (sub1 col) (add1 row)) world)
           1
           0)
       (if (mem (posn col (sub1 row)) world)
           1
           0)
       (if (mem (posn col (add1 row)) world)
           1
           0)
       (if (mem (posn (add1 col) (sub1 row)) world)
           1
           0)
       (if (mem (posn (add1 col) row) world)
           1
           0)
       (if (mem (posn (add1 col) (add1 row)) world)
           1
           0)))))

(define tick_controls
  (λ (world)
    (letrec ([helper
              (λ (col row)
                (cond
                  [(> col NUM_COL) '()]
                  [(> row NUM_ROW) (helper (add1 col) 0)]
;Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;Any live cell with two or three live neighbours lives on to the next generation.
;Any live cell with more than three live neighbours dies, as if by overpopulation.
                  [(mem (posn col row) world)
                   (let ([num_neighbs ((count_live_neighbours world) col row)])
                     (cond
                       [(or (equal? num_neighbs 2)
                            (equal? num_neighbs 3))
                        (cons (posn col row)
                              (helper col (add1 row)))]
                       [else (helper col (add1 row))]))]
;Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
                  [else (if (equal?
                             3
                             ((count_live_neighbours world) col row))
                            (cons (posn col row)
                                  (helper col (add1 row)))
                            (helper col (add1 row)))]))])
      (helper 0 0))))

(define mouse_controls
  (λ (world x y mouse_event)
    (cond
      [(equal? mouse_event "button-down") (tick_controls world)]
      [else world])))

;; use either (init_live_posns fName) or (random_init)
#; ;on mouse
(big-bang random_init
          (to-draw draw_world)
          (on-mouse mouse_controls))

;animated
(big-bang (init_live_posns fName)
          (to-draw draw_world)
          (on-tick tick_controls .25))

;; TESTS
(define test_world
  `(,(posn 5 5) ,(posn 6 5) ,(posn 7 5)))
(define test_world_iter
  (tick_controls test_world))

(check-equal?
 ((count_live_neighbours test_world) 5 5)
 1)

(check-equal?
 ((count_live_neighbours test_world) 6 5)
 2)

(check-equal?
 ((count_live_neighbours test_world) 7 5)
 1)

(check-equal?
 (and (mem (posn 5 5) test_world)
      (mem (posn 6 5) test_world)
      (mem (posn 7 5) test_world))
 #t)

(check-equal?
 (and (mem (posn 6 4) test_world_iter)
      (mem (posn 6 5) test_world_iter)
      (mem (posn 6 6) test_world_iter))
 #t)

(define test_world2
  (init_live_posns "life_init1"))

(check-equal?
 (and (mem (posn 20 20) test_world2)
      (mem (posn 21 20) test_world2)
      (mem (posn 22 20) test_world2)
      (mem (posn 19 21) test_world2)
      (mem (posn 20 21) test_world2)
      (mem (posn 21 21) test_world2))
 #t)