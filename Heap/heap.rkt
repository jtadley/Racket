#lang racket

; functional heap implementation (leftist tree)
; http://typeocaml.com/2015/03/12/heap-leftist-tree/
; min -> O(1)
; insert -> O(logn)
; delete-min -> O(logn)
; merge -> O(logn)

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

(define singleton
  (λ (v)
    (node (leaf) v (leaf) 1)))

(define insert
  (λ (v n)
    (merge (singleton v) n)))

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

  (define balanced?
    (λ (n)
      (cond
        [(leaf? n) #t]
        [else (let ([l (node-left n)]
                    [r (node-right n)])
                (and
                 (<= (abs (- (height l) (height r))) 1)
                 (balanced? l)
                 (balanced? r)))])))

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
  
  #|
  (check-true (balanced? heap0))
  (check-true (balanced? heap1))
  (check-true (balanced? heap2))
  (check-true (balanced? heap3))
  (check-true (balanced? heap4))
  (check-true (balanced? heap5))
  (check-true (balanced? heap6))
  (check-true (balanced? heap10))
  (check-true (balanced? heap11))
  (check-true (balanced? heap12))
  (check-true (balanced? heap13))
  (check-true (balanced? heap14))
  (check-true (balanced? heap15))
|#
  )