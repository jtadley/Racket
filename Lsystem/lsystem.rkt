#lang racket

;; L System



(define snoc
  (λ (elem ls)
    (foldr cons (list elem) ls)))

(define alphabet
  (list #\F #\L #\R #\X #\Y #\+ #\- #\] #\[))

(define axium
  (list))

(define process
  (λ (iterations)
    (letrec
        ([helper
          (λ (out temp_out i)
            (cond
              [(<= i 0) temp_out]
              [(null? out) (helper axium temp_out (sub1 i))]
              ;[production_predecessor production_successor]
              [(member (car out) alphabet) (helper (cdr out) (snoc (car out) temp_out) i)]
              [else (error (format "bad variable in: ~v" (car out)))]))])
      (helper (axium axium iterations)))))



(define X 500)
(define Y 500)
(define DIR 0)
(define SIZE -1.5)
(define GROWTHSIZE 15)
(define ANGLE 90)
(define GROWTHANGLE 0)


(define generate
  (λ (input x y dir size angle)
    (cond
      [(null? input) 'TODO]
      [(eqv? (car input) #\F) 'TODO]
      [(eqv? (car input) #\+) (generate (cdr input) x y (+ dir angle) size angle)]
      [(eqv? (car input) #\-) (generate (cdr input) x y (- dir angle) size angle)]
      [(eqv? (car input) #\>) (generate (cdr input) x y dir (* size (- 1 GROWTHSIZE)) angle)]
      [(eqv? (car input) #\<) (generate (cdr input) x y dir (* size (+ 1 GROWTHSIZE)) angle)]
      [(eqv? (car input) #\)) (generate (cdr input) x y dir size (* angle (+ 1 GROWTHANGLE)))]
      [(eqv? (car input) #\() (generate (cdr input) x y dir size (* angle (- 1 GROWTHANGLE)))]
      [(eqv? (car input) #\[) 'TODO]
      [(eqv? (car input) #\]) 'TODO]
      [(eqv? (car input) #\!) (generate (cdr input) x y dir size (* angle -1))]
      [(eqv? (car input) #\|) (generate (cdr input) x y (+ dir 180) size angle)])))