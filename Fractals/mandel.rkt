#lang racket

(require
  2htdp/image)

(define WIDTH 200)
(define HEIGHT 200)
(define maxN 255)
(define minReal -1.5)
(define maxReal 0)
(define minImaginary -1)
(define maxImaginary 1)

(define to_real
  (λ (x minR maxR)
    (+ (* x (/ (- maxR minR) WIDTH)) minR)))

(define to_imaginary
  (λ (y minI maxI)
    (+ (* y (/ (- maxI minI) HEIGHT)) minI)))

(define get_mandel
  (λ (cr ci max_iterations)
    (letrec ([helper
              (λ (i zr zi)
                (cond
                  [(or (>= (+ (* zr zr) (* zi zi)) 4)
                       (>= i max_iterations))
                   i]
                  [else
                   (helper
                    (add1 i)
                    (+ (- (* zr zr) (* zi zi)) cr)
                    (+ (* 2 zr zi) ci))]))])
      (helper 0 0 0))))

(define color_manip_r
  (λ (n)
    (modulo (inexact->exact (round (+ 0 (* n (sin n))))) 256)))

(define color_manip_g
  (λ (n)
    (modulo (inexact->exact (round (+ 0 (* n (sin n))))) 256)))

(define color_manip_b
  (λ (n)
    (modulo (inexact->exact (round (+ 7 (* n (sin n))))) 256)))

(define mandel
  (λ (x y)
    (cond
      [(>= y HEIGHT) '()]
      [(>= x WIDTH) (mandel 0 (add1 y))]
      [else
       (let* ([n (get_mandel
                  (to_real x minReal maxReal)
                  (to_imaginary y minImaginary maxImaginary)
                  maxN)]
              [r (color_manip_r n)]
              [g (color_manip_g n)]
              [b (color_manip_b n)])
         (cons
          (make-color r b g)
          (mandel (add1 x) y)))])))


(color-list->bitmap (mandel 0 0) WIDTH HEIGHT)