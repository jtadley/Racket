#lang racket

; Jacob Adley

(require
  racket/struct
  2htdp/image
  2htdp/universe)


(define WIDTH 800)
(define HEIGHT 800)
(define node-color (color 255 102 255))
(define node-radius 3)

                             
;           ;                           
;           ;              ;            
;           ;              ;            
;           ;              ;            
;           ;              ;;           
;           ;              ;;;          
;           ;            ;;;;           
;      ;;;  ;     ;;;      ;;      ;;;  
;     ;  ;; ;   ;;   ;     ;;    ;;   ; 
;    ;     ;;        ;     ;;         ; 
;   ;       ;        ;      ;         ; 
;   ;       ;     ;; ;      ;      ;; ; 
;   ;      ;;   ;;  ;;      ;    ;;  ;; 
;   ;     ;;;   ;   ;;      ;    ;   ;; 
;   ;;   ;; ;   ;  ; ;      ;    ;  ; ; 
;     ;;;   ;   ;;;  ;      ;    ;;;  ; 
;           ;                           
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

(define posn-=?
  (λ (p1 p2)
    (and (equal? (posn-x p1) (posn-x p2))
         (equal? (posn-y p1) (posn-y p2)))))

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


                
;       ;;;;   ;;;;;;     ;;;; 
;     ;;    ;  ;;    ;   ;;    
;    ;;     ; ;;     ;  ;      
;   ;;      ;  ;     ;  ;      
;   ;       ;  ;    ;    ;;    
;   ;       ;  ;    ;      ;;  
;   ;      ;   ;   ;         ; 
;   ;      ;   ;;;;          ; 
;    ;    ;    ;        ;;  ;; 
;     ;;;;      ;        ;;;;  
;               ;              
;               ;              
;               ;              
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
                p2)
               (E)
               (E))]
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
                (box xmid ymid xmax ymax (/ (+ xmid xmax) 2) (/ (+ ymid ymax) 2))
                p1
                p2))]
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
               [(posn? nw) (node bounds
                                 (two-posn-node (box xmin ymin xmid ymid (/ (+ xmin xmid) 2) (/ (+ ymin ymid) 2)) p nw)
                                 ne
                                 sw
                                 se)]
               [(E? nw) (node bounds p ne sw se)])]
            ; ne
            [(and (>= x xmid) (< y ymid))
             (cond
               [(node? ne) (node bounds nw (insert p ne) sw se)]
               [(posn? ne) (node bounds
                                 nw
                                 (two-posn-node (box xmid ymin xmax ymid (/ (+ xmid xmax) 2) (/ (+ ymin ymid) 2)) p ne)
                                 sw
                                 se)]
               [(E? ne) (node bounds nw p sw se)])]
            ; sw
            [(and (< x xmid) (>= y ymid))
             (cond
               [(node? sw) (node bounds nw ne (insert p sw) se)]
               [(posn? sw) (node bounds
                                 nw
                                 ne
                                 (two-posn-node (box xmin ymid xmid ymax (/ (+ xmin xmid) 2) (/ (+ ymid ymax) 2)) p sw)
                                 se)]
               [(E? sw) (node bounds nw ne p se)])]
            ; se
            [(and (>= x xmid) (>= y ymid))
             (cond
               [(node? se) (node bounds nw ne sw (insert p se))]
               [(posn? se) (node bounds
                                 nw
                                 ne
                                 sw
                                 (two-posn-node (box xmid ymid xmax ymax (/ (+ xmid xmax) 2) (/ (+ ymid ymax) 2)) p se))]
               [(E? se) (node bounds nw ne sw p)])]
            ;shouldn't get here
            [else (error "could not find a place in box for posn")])))))

; expects a node, posn, or E
(define size
  (λ (v)
    (cond
      [(E? v) 0]
      [(posn? v) 1]
      [(node? v) (+ (size (node-nw v))
                    (size (node-ne v))
                    (size (node-sw v))
                    (size (node-se v)))])))

; a posn and a node or E
(define lookup
  (λ (p v)
    (if (E? v)
        #f
        (let* ([x (posn-x p)]
               [y (posn-y p)]
               [bounds (node-bounds v)]
               [xmin (box-xmin bounds)]
               [ymin (box-ymin bounds)]
               [xmax (box-xmax bounds)]
               [ymax (box-ymax bounds)]
               [xmid (box-xmid bounds)]
               [ymid (box-ymid bounds)]
               [nw (node-nw v)]
               [ne (node-ne v)]
               [sw (node-sw v)]
               [se (node-se v)])
          (cond
            ;nw
            [(and (>= x xmin) (< x xmid) (>= y ymin) (< y ymid))
             (cond
               [(E? nw) #f]
               [(posn? nw) (posn-=? nw p)]
               [(node? nw) (lookup p nw)])]
            ;ne
            [(and (>= x xmid) (< x xmax) (>= y ymin) (< y ymid))
             (cond
               [(E? ne) #f]
               [(posn? ne) (posn-=? ne p)]
               [(node? ne) (lookup p ne)])]
            ;sw
            [(and (>= x xmin) (< x xmid) (>= y ymid) (< y ymax))
             (cond
               [(E? sw) #f]
               [(posn? sw) (posn-=? sw p)]
               [(node? sw) (lookup p sw)])]
            ;se
            [(and (>= x xmid) (< x xmax) (>= y ymid) (< y ymax))
             (cond
               [(E? se) #f]
               [(posn? se) (posn-=? se p)]
               [(node? se) (lookup p se)])]
            [else #f])))))

(define init
  (node (box 0 0 WIDTH HEIGHT (/ WIDTH 2) (/ HEIGHT 2))
        (E)
        (E)
        (E)
        (E)))


                                   
;                                            
;      ;                         ;           
;      ;                         ;           
;      ;                         ;           
;      ;;                        ;;          
;      ;;;                       ;;;         
;    ;;;;                      ;;;;          
;      ;;     ;;;;      ;;;;     ;;     ;;;; 
;      ;;    ;;  ;     ;;        ;;    ;;    
;      ;;    ;    ;   ;          ;;   ;      
;       ;   ;    ;;   ;           ;   ;      
;       ;   ;;;;;      ;;         ;    ;;    
;       ;   ;            ;;       ;      ;;  
;       ;   ;      ;       ;      ;        ; 
;       ;    ;   ;;        ;      ;        ; 
;       ;     ;;;;    ;;  ;;      ;   ;;  ;; 
;                      ;;;;            ;;;;  
(module+ test
  (require rackunit)

  (check-equal?
   (size init)
   0)

  (define node1
    (insert (posn 3 4)
                 (insert (posn 4 3)
                         init)))

  (check-equal?
   (size node1)
   2)
  (check-true
   (lookup (posn 3 4) node1))
  (check-true
   (lookup (posn 4 3) node1))
  (check-false
   (lookup (posn 3 3) node1))
  (check-false
   (lookup (posn 4 4) node1)))


                                                                                              
;           ;                                                                           ;           ; 
;           ;                                                                           ;           ; 
;           ;                                                                           ;           ; 
;           ;                                                                           ;           ; 
;           ;                                                                           ;           ; 
;           ;                                                                           ;           ; 
;           ;                                                                           ;           ; 
;      ;;;  ;   ;         ;;;   ;        ;                ;        ;     ;;;;   ;       ;      ;;;  ; 
;     ;  ;; ;   ;   ;;  ;;   ;  ;       ;                 ;       ;    ;;    ;  ;   ;;  ;     ;  ;; ; 
;    ;     ;;   ;  ;         ;  ;       ;   ;;            ;       ;   ;;     ;  ;  ;    ;    ;     ;; 
;   ;       ;   ; ;;         ;  ;       ;   ;;;;;;;;;;;   ;       ;  ;;      ;  ; ;;    ;   ;       ; 
;   ;       ;   ;;;       ;; ;   ;  ;   ;                  ;  ;   ;  ;       ;  ;;;     ;   ;       ; 
;   ;      ;;   ;;      ;;  ;;   ; ; ; ;                   ; ; ; ;   ;       ;  ;;      ;   ;      ;; 
;   ;     ;;;    ;      ;   ;;   ; ;  ;;                   ; ;  ;;   ;      ;    ;      ;   ;     ;;; 
;   ;;   ;; ;    ;      ;  ; ;   ;;   ;;                   ;;   ;;   ;      ;    ;      ;   ;;   ;; ; 
;     ;;;   ;    ;      ;;;  ;    ;    ;                    ;    ;    ;    ;     ;            ;;;   ; 
;           ;                                                          ;;;;                         ; 
(define get-color
  (λ (depth)
    (cond
      [(eqv? depth 0) (color 229 204 255)]
      [(eqv? depth 1) (color 204 153 255)]
      [(eqv? depth 2) (color 178 102 255)]
      [(eqv? depth 3) (color 153 51 255)]
      [(eqv? depth 4) (color 127 0 255)]
      [(eqv? depth 5) (color 102 0 204)]
      [(eqv? depth 6) (color 76 0 153)]
      [(eqv? depth 7) (color 51 0 102)]
      [else (color 25 0 51)])))

(define draw-world
  (λ (world)
    (define scene (empty-scene WIDTH HEIGHT))
    (letrec ([helper
              (λ (n depth)
                (let ([bounds (node-bounds n)]
                      [nw (node-nw n)]
                      [ne (node-ne n)]
                      [sw (node-sw n)]
                      [se (node-se n)])
                  (set! scene (place-image
                               (rectangle
                                (- (box-xmax bounds)
                                   (box-xmin bounds))
                                (- (box-ymax bounds)
                                   (box-ymin bounds))
                                "solid"
                                (get-color depth))
                               (box-xmid bounds)
                               (box-ymid bounds)
                               scene))
                  (if (node? nw)
                      (helper nw (add1 depth))
                      (void))
                  (if (node? ne)
                      (helper ne (add1 depth))
                      (void))
                  (if (node? sw)
                      (helper sw (add1 depth))
                      (void))
                  (if (node? se)
                      (helper se (add1 depth))
                      (void))
                  (if (posn? nw)
                      (set! scene (place-image
                                   (circle
                                    node-radius
                                    "solid"
                                    node-color)
                                   (+ (posn-x nw) (/ node-radius 2))
                                   (+ (posn-y nw) (/ node-radius 2))
                                   scene))
                      (void))
                  (if (posn? ne)
                      (set! scene (place-image
                                   (circle
                                    node-radius
                                    "solid"
                                    node-color)
                                   (+ (posn-x ne) (/ node-radius 2))
                                   (+ (posn-y ne) (/ node-radius 2))
                                   scene))
                      (void))
                  (if (posn? sw)
                      (set! scene (place-image
                                   (circle
                                    node-radius
                                    "solid"
                                    node-color)
                                   (+ (posn-x sw) (/ node-radius 2))
                                   (+ (posn-y sw) (/ node-radius 2))
                                   scene))
                      (void))
                  (if (posn? se)
                      (set! scene (place-image
                                   (circle
                                    node-radius
                                    "solid"
                                    node-color)
                                   (+ (posn-x se) (/ node-radius 2))
                                   (+ (posn-y se) (/ node-radius 2))
                                   scene))
                      (void))))])
      (helper world 0)
      scene)))

                                              
;       ;;  ;;        ;;;;  ;     ;     ;;;;    ;;;;   
;     ;; ;  ; ;     ;;    ; ;     ;    ;;      ;;  ;   
;     ;; ; ;  ;    ;;     ; ;     ;   ;        ;    ;  
;     ;   ;;  ;   ;;      ; ;     ;   ;       ;    ;;  
;     ;   ;;  ;   ;       ; ;    ;;    ;;     ;;;;;    
;     ;   ;   ;   ;       ; ;    ;;      ;;   ;        
;     ;   ;   ;   ;      ;  ;   ;;;        ;  ;      ; 
;     ;   ;   ;   ;      ;  ;  ;  ;        ;   ;   ;;  
;     ;   ;   ;    ;    ;   ;  ;  ;   ;;  ;;    ;;;;   
;                   ;;;;     ;;   ;    ;;;;            
(define mouse-controls
  (λ (world x y mouse_event)
    (cond
      [(equal? mouse_event "button-down") (if (not (lookup (posn x y) world))
                                              (insert (posn x y) world)
                                              world)]
      [else world])))
                                                                          
;   ;                                       ;                                      
;   ;          ;                            ;                                      
;   ;                                       ;                                      
;   ;                                       ;                                      
;   ;                 ;;;;                  ;                   ;            ;;;;  
;   ;    ;;;   ;    ;;    ;                 ;    ;;;     ;;;    ;   ;;     ;;    ; 
;    ;  ;   ;  ;   ;;     ;                  ;  ;   ;  ;;   ;   ;   ; ;   ;;     ; 
;    ;;;    ;  ;   ;      ;   ;;             ;;;    ;       ;   ;  ;  ;   ;      ; 
;    ;;     ;  ;  ;      ;;   ;;;;;;;;;;;    ;;     ;       ;   ; ;   ;  ;      ;; 
;    ;      ;  ;  ;      ;;                  ;      ;    ;; ;    ;;   ;  ;      ;; 
;    ;     ;   ;  ;     ; ;                  ;     ;   ;;  ;;    ;    ;  ;     ; ; 
;    ;    ;    ;  ;;   ;  ;                  ;    ;    ;   ;;    ;    ;  ;;   ;  ; 
;    ;   ;     ;    ;;;   ;                  ;   ;     ;  ; ;    ;    ;    ;;;   ; 
;     ;;;      ;          ;                   ;;;      ;;;  ;    ;    ;          ; 
;                    ;    ;                                                 ;    ; 
;                    ;    ;                                                 ;    ; 
;                     ;;  ;                                                  ;;  ; 
;                       ;;;                                                    ;;; 
(define bang
  (λ ()
    (big-bang init
      (to-draw draw-world)
      (on-mouse mouse-controls))))