#lang racket

; functional heap implementation (leftist tree)
; http://typeocaml.com/2015/03/12/heap-leftist-tree/
; min -> O(1)
; insert -> O(logn)
; delete-min -> O(logn)
; merge -> O(logn)

;merge2 is the same as merge, but might be easier to implement in coq

;abstraction: list
;insert -> cons
;getmin -> min
;merge -> append

(require racket/struct)

(struct leaf
  ())

(struct node
  (left
   value
   right
   rank ;length of path between node and rightmost leaf
   )
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (n) 'node)
      (λ (n) (list (node-left n)                   
                   (node-value n)                   
                   (node-right n)
                   (node-rank n)))))])

(define rank
  (λ (n)
    (if (leaf? n)
        0
        (node-rank n))))

(define height
  (λ (n)
    (if (leaf? n)
        0
        (add1 (max (height (node-left n))
                   (height (node-right n)))))))

(define size
  (λ (n)
    (if (leaf? n)
        0
        (add1 (+ (size (node-left n))
                 (size (node-right n)))))))

(define merge
  (λ (n1 n2)
    (cond
      [(leaf? n1) n2]
      [(leaf? n2) n1]
      [else
       (let ([n1-l (node-left n1)]
             [n1-v (node-value n1)]
             [n1-r (node-right n1)]
             [n2-v (node-value n2)])
         (if (> n1-v n2-v)
             (merge n2 n1)
             (let* ([merged (merge n1-r n2)]
                    [n1-l-rank (rank n1-l)]
                    [merged-rank (node-rank merged)])
               (if (>= n1-l-rank merged-rank)
                   (node n1-l n1-v merged (add1 merged-rank))
                   (node merged n1-v n1-l (add1 n1-l-rank))))))])))

(define merge2
  (λ (n1 n2)
    (cond
      [(leaf? n1) n2]
      [(leaf? n2) n1]
      [else
       (let ([n1-l (node-left n1)]
             [n1-v (node-value n1)]
             [n1-r (node-right n1)]
             [n2-l (node-left n2)]
             [n2-v (node-value n2)]
             [n2-r (node-right n2)])
         (if (> n1-v n2-v)
             (if (>= (rank n2-l) (node-rank (merge n2-r n1)))
                 (node n2-l n2-v (merge n2-r n1) (add1 (node-rank (merge n2-r n1))))
                 (node (merge n2-r n1) n2-v n2-l (add1 (rank n2-l))))
             (if (>= (rank n1-l) (node-rank (merge n1-r n2)))
                 (node n1-l n1-v (merge n1-r n2) (add1 (node-rank (merge n1-r n2))))
                 (node (merge n1-r n2) n1-v n1-l (add1 (rank n1-l))))))])))

(define singleton
  (λ (v)
    (node (leaf) v (leaf) 1)))

(define insert
  (λ (v n)
    (merge (singleton v) n)))

(define insert2
  (λ (v n)
    (merge2 (singleton v) n)))

(define min
  (λ (n)
    (node-value n)))

(define delete-min
  (λ (n)
    (merge (node-left n) (node-right n))))

(module+ test
  (require rackunit)

  (define heap?
    (λ (n)
      (cond
        [(leaf? n) #t]
        [else (let ([l (node-left n)]
                    [r (node-right n)]
                    [v (node-value n)])
                (and
                 (if (node? l)
                     (<= v (node-value l))
                     #t)
                 (if (node? r)
                     (<= v (node-value r))
                     #t)
                 (heap? l)
                 (heap? r)))])))

  (define leftist?
    (λ (n)
      (cond
        [(leaf? n) #t]
        [else (let ([l (node-left n)]
                    [r (node-right n)])
                (and
                 (<= (rank r) (rank l))
                 (leftist? l)
                 (leftist? r)))])))

  (define length-to-rightmost-leaf
    (λ (n)
      (cond
        [(leaf? n) 0]
        [else (add1 (length-to-rightmost-leaf (node-right n)))])))

  (define correct-ranks?
    (λ (n)
      (cond
        [(leaf? n) #t]
        [else
         (and (equal? (length-to-rightmost-leaf n) (node-rank n))
              (correct-ranks? (node-left n))
              (correct-ranks? (node-right n)))])))

  (define heap0 (leaf))
  (define heap1 (singleton 1))
  (define heap2 (insert 2 heap1))
  (define heap3 (merge heap2 heap2))
  (define heap4 (insert -1 heap3))
  (define heap5 (insert 5 heap4))
  (define heap6 (insert -5 heap5))
  (define heap10 (singleton 4))
  (define heap11 (insert 3 heap10))
  (define heap12 (insert -1 heap11))
  (define heap13 (insert -3 heap12))
  (define heap14 (insert 0 heap13))
  (define heap15 (merge heap6 heap14))

  (check-equal? (size heap0) 0)
  (check-equal? (size heap1) 1)
  (check-equal? (size heap2) 2)
  (check-equal? (size heap3) 4)
  (check-equal? (size heap4) 5)
  (check-equal? (size heap5) 6)
  (check-equal? (size heap6) 7)
  (check-equal? (size heap10) 1)
  (check-equal? (size heap11) 2)
  (check-equal? (size heap12) 3)
  (check-equal? (size heap13) 4)
  (check-equal? (size heap14) 5)
  (check-equal? (size heap15) 12)
  
  (check-true (heap? heap0))
  (check-true (heap? heap1))
  (check-true (heap? heap2))
  (check-true (heap? heap3))
  (check-true (heap? heap4))
  (check-true (heap? heap5))
  (check-true (heap? heap6))
  (check-true (heap? heap10))
  (check-true (heap? heap11))
  (check-true (heap? heap12))
  (check-true (heap? heap13))
  (check-true (heap? heap14))
  (check-true (heap? heap15))

  (check-true (leftist? heap0))
  (check-true (leftist? heap1))
  (check-true (leftist? heap2))
  (check-true (leftist? heap3))
  (check-true (leftist? heap4))
  (check-true (leftist? heap5))
  (check-true (leftist? heap6))
  (check-true (leftist? heap10))
  (check-true (leftist? heap11))
  (check-true (leftist? heap12))
  (check-true (leftist? heap13))
  (check-true (leftist? heap14))
  (check-true (leftist? heap15))

  (check-true (correct-ranks? heap0))
  (check-true (correct-ranks? heap1))
  (check-true (correct-ranks? heap2))
  (check-true (correct-ranks? heap3))
  (check-true (correct-ranks? heap4))
  (check-true (correct-ranks? heap5))
  (check-true (correct-ranks? heap6))
  (check-true (correct-ranks? heap10))
  (check-true (correct-ranks? heap11))
  (check-true (correct-ranks? heap12))
  (check-true (correct-ranks? heap13))
  (check-true (correct-ranks? heap14))
  (check-true (correct-ranks? heap15))

  (define heap0^ (leaf))
  (define heap1^ (singleton 1))
  (define heap2^ (insert2 2 heap1))
  (define heap3^ (merge2 heap2 heap2))
  (define heap4^ (insert2 -1 heap3))
  (define heap5^ (insert2 5 heap4))
  (define heap6^ (insert2 -5 heap5))
  (define heap10^ (singleton 4))
  (define heap11^ (insert2 3 heap10))
  (define heap12^ (insert2 -1 heap11))
  (define heap13^ (insert2 -3 heap12))
  (define heap14^ (insert2 0 heap13))
  (define heap15^ (merge2 heap6 heap14))

  (check-equal? (size heap0^) 0)
  (check-equal? (size heap1^) 1)
  (check-equal? (size heap2^) 2)
  (check-equal? (size heap3^) 4)
  (check-equal? (size heap4^) 5)
  (check-equal? (size heap5^) 6)
  (check-equal? (size heap6^) 7)
  (check-equal? (size heap10^) 1)
  (check-equal? (size heap11^) 2)
  (check-equal? (size heap12^) 3)
  (check-equal? (size heap13^) 4)
  (check-equal? (size heap14^) 5)
  (check-equal? (size heap15^) 12)
  
  (check-true (heap? heap0^))
  (check-true (heap? heap1^))
  (check-true (heap? heap2^))
  (check-true (heap? heap3^))
  (check-true (heap? heap4^))
  (check-true (heap? heap5^))
  (check-true (heap? heap6^))
  (check-true (heap? heap10^))
  (check-true (heap? heap11^))
  (check-true (heap? heap12^))
  (check-true (heap? heap13^))
  (check-true (heap? heap14^))
  (check-true (heap? heap15^))

  (check-true (leftist? heap0^))
  (check-true (leftist? heap1^))
  (check-true (leftist? heap2^))
  (check-true (leftist? heap3^))
  (check-true (leftist? heap4^))
  (check-true (leftist? heap5^))
  (check-true (leftist? heap6^))
  (check-true (leftist? heap10^))
  (check-true (leftist? heap11^))
  (check-true (leftist? heap12^))
  (check-true (leftist? heap13^))
  (check-true (leftist? heap14^))
  (check-true (leftist? heap15^))

  (check-true (correct-ranks? heap0^))
  (check-true (correct-ranks? heap1^))
  (check-true (correct-ranks? heap2^))
  (check-true (correct-ranks? heap3^))
  (check-true (correct-ranks? heap4^))
  (check-true (correct-ranks? heap5^))
  (check-true (correct-ranks? heap6^))
  (check-true (correct-ranks? heap10^))
  (check-true (correct-ranks? heap11^))
  (check-true (correct-ranks? heap12^))
  (check-true (correct-ranks? heap13^))
  (check-true (correct-ranks? heap14^))
  (check-true (correct-ranks? heap15^))

  (define heap20 (singleton 0))
  (define heap21 (insert2 0 heap20))
  (define heap22 (insert2 0 heap21))
  (define heap23 (insert2 0 heap22))
  (define heap24 (merge2 heap23 heap23))
  (define heap25 (merge2 heap24 heap24))
  (define heap26 (insert2 0 heap25))
  (define heap27 (insert2 0 heap26))
  (define heap28 (insert2 0 heap27))

  (check-equal? (size heap20) 1)
  (check-equal? (size heap21) 2)
  (check-equal? (size heap22) 3)
  (check-equal? (size heap23) 4)
  (check-equal? (size heap24) 8)
  (check-equal? (size heap25) 16)
  (check-equal? (size heap26) 17)
  (check-equal? (size heap27) 18)
  (check-equal? (size heap28) 19)

  (check-true (heap? heap20))
  (check-true (heap? heap21))
  (check-true (heap? heap22))
  (check-true (heap? heap23))
  (check-true (heap? heap24))
  (check-true (heap? heap25))
  (check-true (heap? heap26))
  (check-true (heap? heap27))
  (check-true (heap? heap28))

  (check-true (leftist? heap20))
  (check-true (leftist? heap21))
  (check-true (leftist? heap22))
  (check-true (leftist? heap23))
  (check-true (leftist? heap24))
  (check-true (leftist? heap25))
  (check-true (leftist? heap26))
  (check-true (leftist? heap27))
  (check-true (leftist? heap28))

  (check-true (correct-ranks? heap20))
  (check-true (correct-ranks? heap21))
  (check-true (correct-ranks? heap22))
  (check-true (correct-ranks? heap23))
  (check-true (correct-ranks? heap24))
  (check-true (correct-ranks? heap25))
  (check-true (correct-ranks? heap26))
  (check-true (correct-ranks? heap27))
  (check-true (correct-ranks? heap28))
  )