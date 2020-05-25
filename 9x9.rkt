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
       (if (and s (set-member? s n))
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

;; difficulty 0,1,2,3
;; -1 = 0
;;  0 ~ 42
;;  1 ~ 51
;;  2 ~ 56
;;  3 ~ 58
(define set-difficulty
  (λ (m i)
     (letrec ([n (if (zero? (add1 i))
                   0
                   (+ (random -2 3)
                      (cond
                        [(equal? i 0) 40]
                        [(equal? i 1) 50]
                        [(equal? i 2) 55]
                        [(equal? i 3) 56])))]
              [helper
                (λ (m n)
                   (cond
                     [(zero? n) m]
                     [else
                       (let ([x (random 0 SIZE)]
                             [y (random 0 SIZE)])
                         (if (void? (vector-ref (vector-ref m y) x))
                           (helper m n)
                           (begin
                             (vector-set!-xy m x y (void))
                             (helper m (sub1 n))))) ]))])
       (helper (gen (make-empty) 0 0 (set)) n))))

(define run 
  (λ ()
     (define m (make-empty))
     (gen m 0 0 (set))
     (set-difficulty m -1)
     ))

(run)
