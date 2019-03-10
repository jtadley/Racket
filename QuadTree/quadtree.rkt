#lang racket

; Jacob Adley

(require
  racket/struct)

(struct posn
  (x ;number
   y ;number
   )
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (p) 'posn)
      (λ (p) (list (posn-x p)
                   (posn-y p)))))])

(struct box
  (xmin ;number
   ymin ;number
   xmax ;number
   ymax ;number
   xmid ;number
   ymid ;number
   )
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (p) 'box)
      (λ (p) (list (box-xmin p)
                   (box-ymin p)
                   (box-xmax p)
                   (box-ymax p)))))])

(struct E
  ())

(struct node
  (bounds ;box
   nw ;node OR posn OR E
   ne ;node OR posn OR E
   sw ;node OR posn OR E
   se ;node OR posn OR E
   )
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (n) 'node)
      (λ (n) (list (node-bounds n)
                   (node-nw n)
                   (node-ne n)
                   (node-sw n)
                   (node-se n)))))])

(define two-posn-node
  (λ (bounds p1 p2)
    (let* ([x1 (posn-x p1)]
           [y1 (posn-y p1)]
           [x2 (posn-x p2)]
           [y2 (posn-y p2)]
           [xmin (box-xmin bounds)]
           [ymin (box-ymin bounds)]
           [xmax (box-xmax bounds)]
           [ymax (box-ymax bounds)]
           [xmid (box-xmid bounds)]
           [ymid (box-ymid bounds)]
           [p1-nw (and (< x1 xmid) (< y1 ymid))]
           [p2-nw (and (< x2 xmid) (< y2 ymid))]
           [p1-ne (and (>= x1 xmid) (< y1 ymid))]
           [p2-ne (and (>= x2 xmid) (< y2 ymid))]
           [p1-sw (and (< x1 xmid) (>= y1 ymid))]
           [p2-sw (and (< x2 xmid) (>= y2 ymid))]
           [p1-se (and (>= x1 xmid) (>= y1 ymid))]
           [p2-se (and (>= x2 xmid) (>= y2 ymid))])
      (cond
        ; both in nw
        [(and p1-nw p2-nw)
         (node bounds
               (two-posn-node
                (box xmin ymin xmid ymid (/ (+ xmin xmid) 2) (/ (+ ymin ymid) 2))
                p1
                p2)
               (E)
               (E)
               (E))]
        ; both in ne
        [(and p1-ne p2-ne)
         (node bounds
               (E)
               (two-posn-node
                (box xmid ymin xmax ymid (/ (+ xmid xmax) 2) (/ (+ ymin ymid) 2))
                p1
                p2
                (E)
                (E)))]
        ; both in sw
        [(and p1-sw p2-sw)
         (node bounds
               (E)
               (E)
               (two-posn-node
                (box xmin ymid xmid ymax (/ (+ xmin xmid) 2) (/ (+ ymid ymax) 2))
                p1
                p2)
               (E))]
        ; both in se
        [(and p1-se p2-se)
         (node bounds
               (E)
               (E)
               (E)
               (two-posn-node
                (box xmid ymid xmax ymax (/ (+ xmid xmax) 2) (/ (+ ymid ymax) 2))))]
        [else
         (cond
           [p1-nw
            (insert p2
                    (node bounds p1 (E) (E) (E)))]
           [p1-ne
            (insert p2
                    (node bounds (E) p1 (E) (E)))]
           [p1-sw
            (insert p2
                    (node bounds (E) (E) p1 (E)))]
           [p1-se
            (insert p2
                    (node bounds (E) (E) (E) p1))])]))))

; insert a p:posn into a n:node
(define insert
  (λ (p n)
    (if (E? n)
        (error "got to E on insert")
        (let* ([x (posn-x p)]
               [y (posn-y p)]
               [bounds (node-bounds n)]
               [xmin (box-xmin bounds)]
               [ymin (box-ymin bounds)]
               [xmax (box-xmax bounds)]
               [ymax (box-ymax bounds)]
               [xmid (box-xmid bounds)]
               [ymid (box-ymid bounds)]
               [nw (node-nw n)]
               [ne (node-ne n)]
               [sw (node-sw n)]
               [se (node-se n)])
          (cond
            [(or (< x xmin) (< y ymin) (>= x xmax) (>= y ymax))
             (error "insertion out of bounds of box")]
            ; nw
            [(and (< x xmid) (< y ymid))
             (cond
               [(node? nw) (node bounds (insert p nw) ne sw se)]
               [(posn? nw) (two-posn-node (box xmin ymin xmid ymid (/ (+ xmin xmid) 2) (/ (+ ymin ymid) 2)) p nw)]
               [(E? nw) (node bounds p ne sw se)])]
            ; ne
            [(and (>= x xmid) (< y ymid))
             (cond
               [(node? ne) (node bounds nw (insert p ne) sw se)]
               [(posn? ne) (two-posn-node p ne)]
               [(E? ne) (node bounds nw p sw se)])]
            ; sw
            [(and (< x xmid) (>= y ymid))
             (cond
               [(node? sw) (node bounds nw ne (insert p sw) se)]
               [(posn? sw) (two-posn-node p sw)]
               [(E? sw) (node bounds nw ne p se)])]
            ; se
            [(and (>= x xmid) (>= y ymid))
             (cond
               [(node? se) (node bounds nw ne sw (insert p se))]
               [(posn? se) (two-posn-node p se)]
               [(E? se) (node bounds nw ne sw p)])]
            ;shouldn't get here
            [else (error "could not find a place in box for posn")])))))

(define init
  (node (box 0 0 800 800 400 400)
        (E)
        (E)
        (E)
        (E)))

(module+ test
  (require rackunit)

  (insert (posn 3 3)
          (insert (posn 4 4)
                  init)))