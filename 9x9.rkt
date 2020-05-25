#lang racket

(require racket/set)


(define SIZE 9)


(define divide
  (λ (n m) (inexact->exact (floor (/ n m)))))


(define valid-box?
  (λ (m n)
     (letrec ([side-len (sqrt SIZE)]
              [base-row (* (divide n side-len) 3)]
              [helper
                (λ (p s)
                   (cond
                     [(>= p SIZE) #t]
                     [else (let* ([x (modulo p side-len)]
                                  [y (+ (divide p side-len) base-row)]
                                  [elem (vector-ref (vector-ref m y) x)])
                             (if (and (not (void? elem)) (set-member? s elem))
                               #f
                               (helper (add1 p) (set-add s elem))))]))])
       (helper 0 (set)))))

(define valid-col?
  (λ (m x)
     (letrec ([helper
                (λ (y s)
                   (cond
                     [(>= y SIZE) #t]
                     [else (let ([elem (vector-ref (vector-ref m y) x)])
                             (if (and (not (void? elem)) (set-member? s elem))
                               #f
                               (helper (add1 y) (set-add s elem))))]))])
       (helper 0 (set)))))

(define valid-row?
  (λ (m y)
     (letrec ([row (vector-ref m y)]
              [helper
                (λ (x s)
                   (cond
                     [(>= x SIZE) #t]
                     [else (let ([elem (vector-ref row x)])
                             (if (and (not (void? elem)) (set-member? s elem))
                               #f
                               (helper (add1 x) (set-add s elem))))]))])
       (helper 0 (set)))))

(define valid?
  (λ (m)
    (letrec ([helper
               (λ (n)
                  (cond
                    [(>= n SIZE) #t]
                    [else (and (valid-row? m n)
                               (valid-col? m n)
                               (valid-box? m n)
                               (helper (add1 n)))]))])
      (helper 0))))

(define get-random
  (λ (s) 
     (let ([n (random 0 (add1 SIZE))])
       (if (set-member? s n)
         (get-random s)
         n)))) 

(define vector-set!-xy
  (λ (m x y v)
     (vector-set! (vector-ref m y) x v)))

(define gen
  (λ (m x y tried)
     (cond
       [(>= y SIZE) m]
       [(>= x SIZE) (gen m 0 (add1 y) tried)]
       [(>= (set-count tried) SIZE) #f]
       [else
         (let ([try (get-random tried)])
           (vector-set!-xy m x y try)
           (if (valid? m)
             (or
               (gen m (add1 x) y (set))
               (gen m x y (set-add tried try)))
             (begin 
               (vector-set!-xy m x y (void))
               (gen m x y (set-add tried try)))))])))

(define make-empty
  (λ () 
     (letrec ([helper 
                (λ (n m)
                   (if (>= n SIZE)
                     m
                     (begin
                       (vector-set! m n (make-vector SIZE (void)))
                       (helper (add1 n) m))))])
       (helper 0 (make-vector SIZE)))))

(define run 
  (λ ()
     (define m (make-empty))
     (gen m 0 0 (set))))

(run)
