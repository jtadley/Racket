#lang racket/base

(require racket/match
         racket/list
         define-with-spec
         rackunit)

;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Assignment "lang2b"
;; due 30 Jan by 3pm
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(define Id? symbol?)


(define/spec (reserved-symbol? x)
  (-> symbol? boolean?)
  (and (member x '(λ zero add1 which-Nat iter-Nat rec-Nat)) #t))

;; a predicate for Exprs
;; Expr ::= Id | (λ (Id) Expr) | (Expr Expr) | zero | (add1 Expr) | (which-Nat Expr Expr Expr) | (iter-Nat Expr Expr Expr) | (rec-Nat Expr Expr Expr)
(define (Expr? e)
  (match e
    ['zero #t]
    [`(add1 ,e) (Expr? e)]
    [(? Id? x) (not (reserved-symbol? x))]
    [`(which-Nat ,tgt ,base ,step)
     (and (Expr? tgt)
          (Expr? base)
          (Expr? step))]
    [`(iter-Nat ,tgt ,base ,step)
     (and (Expr? tgt)
          (Expr? base)
          (Expr? step))]
    [`(rec-Nat ,tgt ,base ,step)
     (and (Expr? tgt)
          (Expr? base)
          (Expr? step))]
    [`(λ (,x) ,body)
     (and (Id? x)
          (not (reserved-symbol? x))
          (Expr? body))]
    [`(,rator ,rand)
     (and (Expr? rator)
          (Expr? rand))]
    [_ #f]))





;; list of id (List Id)
(define ListId?
  (λ (ls)
    (and (list? ls)
         (andmap (λ (entry) (Id? entry))
                 ls))))

;; tests for ListId?
(check-equal?
 (ListId? (list 'x 'y 'z 'w))
 #t)


;; NormalExpr ::= NeutralExpr | (λ (Id) NormalExpr) | zero | (add1 Expr)
(define NormalExpr?
  (λ (x) (match x
           [`,x #:when (NeutralExpr? x) #t]
           [`(λ ,listId ,n-exp)
            (and (ListId? listId)
                 (NormalExpr? n-exp))]
           ['zero #t]
           [`(add1 ,n) (NormalExpr? n)]
           [_ #f])))

;; NeutralExpr ::= Id | (NeutralExpr NormalExpr) | zero | (add1 Expr) | (which-Nat/iter-Nat/rec-Nat NeutralExpr NormalExpr NormalExpr)
(define NeutralExpr?
  (λ (x) (or (Id? x)
             (match x
               [(? Id? x) (not (reserved-symbol? x))]
               [`(,rator ,rand) (and (NeutralExpr? rator)
                                     (NormalExpr? rand))]
               ['zero #t]
               [`(add1 ,y) (NeutralExpr? y)]
               [`(which-Nat ,tgt ,base ,step)
                (and (NeutralExpr? tgt)
                     (NormalExpr? base)
                     (NormalExpr? step))]
               [`(iter-Nat ,tgt ,base ,step)
                (and (NeutralExpr? tgt)
                     (NormalExpr? base)
                     (NormalExpr? step))]
               [`(rec-Nat ,tgt ,base ,step)
                (and (NeutralExpr? tgt)
                     (NormalExpr? base)
                     (NormalExpr? step))]
               [`(,neutral-exp ,normal-exp)
                (and (NeutralExpr? neutral-exp)
                     (NormalExpr? normal-exp))]
               [_ #f]))))


;;tests for NormalExpr? and NeutralExpr?
(check-equal?
 (NormalExpr? '(λ (x) x))
 #t)
(check-equal?
 (NormalExpr? 'x)
 #t)
(check-equal?
 (NormalExpr? '(λ (x) (λ (x) (λ (y) (y x)))))
 #t)
(check-equal?
 (NeutralExpr? '(someArbitraryThing (λ (x) anotherArbitraryThing)))
 #t)
(check-equal?
 (NormalExpr? '(iter-Nat zero
                         x
                         (λ (y) x)))
 #t)
(check-equal?
 (NeutralExpr? '(iter-Nat x
                          y
                          (λ (z) y)))
 #t)
(check-equal?
 (NormalExpr? '(iter-Nat x
                         y
                         (λ (z) y)))
 #t)


;; a closure (i.e. function value)
(struct/spec CLOSURE ([ρ ValueEnv?]
                      [id Id?]
                      [body Expr?]))

;; ρ is a ValueEnv?
;; id is an Id?
;; body is an Expr?
;; NOTE: when we define a struct with struct/spec,
;; it defines functions that access the respective
;; fields, in this case:
;; - CLOSURE-ρ
;; - CLOSURE-id
;; - CLOSURE-body
;; along with a constructor and predicate:
;; - CLOSURE
;; - CLOSURE?


(struct/spec ZERO ())
(struct/spec ADD1 ([body (either ZERO?
                                 ADD1?
                                 N-Val?)]))

(define NUMBER? (λ (n) (or (ADD1? n)
                           (ZERO? n))))

;; a Value is a CLOSURE
(define Value? (λ (x)
                 (or (NUMBER? x)
                     (CLOSURE? x)
                     (N-Val? x))))



;; a ValueEnv is a (listof (list Id? Value?))
(define (ValueEnv? ρ)
  (and (list? ρ)
       (andmap (λ (entry) (and (= 2 (length entry))
                               (Id? (first entry))
                               (Value? (second entry))))
               ρ)))



;; a Neutral identifier -- an intermediate
;; part of the NbE process
(struct/spec N-ID ([name Id?]))


;; a Neutral application -- an intermediate
;; part of the NbE process
(struct/spec N-APP ([rator N-Val?]
                    [rand Value?]))

;; Neutral Nat eliminator forms
(struct/spec N-WHICH-NAT ([tgt N-Val?]
                          [base Value?]
                          [step Value?]))


(struct/spec N-ITER-NAT ([tgt N-Val?]
                         [base Value?]
                         [step Value?]))


(struct/spec N-REC-NAT ([tgt N-Val?]
                        [base Value?]
                        [step Value?]))


;; Neutral terms that appear during
;; NbE conversion
(define N-Val? (λ (x) (or (N-ID? x)
                          (N-APP? x)
                          (N-WHICH-NAT? x)
                          (N-ITER-NAT? x)
                          (N-REC-NAT? x))))


;; tests for N-ID, N-APP, N-Val?
(define n-id
  (N-ID 'lentils))
(define n-app1
  (N-APP n-id (ADD1 (ZERO))))
(define n-app2
  (N-APP n-app1 n-id))
(check-equal?
 (N-Val? n-id)
 #t)
(check-equal?
 (N-Val? n-app1)
 #t)
(check-equal?
 (N-Val? n-app2)
 #t)
(check-equal?
 (N-APP-rator n-app2)
 n-app1)
(check-equal?
 (N-APP-rand n-app2)
 n-id)
(check-equal?
 (N-ID-name (N-APP-rand n-app2))
 'lentils)
(check-equal?
 (N-APP-rator (N-APP-rator n-app2))
 n-id)
(check-equal?
 (N-APP-rand (N-APP-rator n-app2))
 (ADD1 (ZERO)))


;; lookup the value of x in ρ
(define/spec (lookup ρ x)
  (-> ValueEnv? Id? (either Value? #f))
  (match (assoc x ρ)
    ;; this match pattern matches
    ;; when the value is the literal #f
    [#f #f]
    [`(,_ ,val) val]))

;; extend ρ with the mapping from x to v
(define/spec (extend ρ x v)
  (-> ValueEnv? Id? Value? ValueEnv?)
  (cons (list x v) ρ))



(define/spec (valof ρ e)
  (-> ValueEnv? Expr? Value?)
  (match e
    [`zero (ZERO)]
    [`(add1 ,n) (ADD1 (valof ρ n))]
    [`,y #:when (and (Id? y) (not (reserved-symbol? y))) (or (lookup ρ y)
                                                             (error 'valof "unbound identifier ~v" y))]
    [`(which-Nat ,tgt ,base ,step)
     (elim-which-Nat (valof ρ tgt) (valof ρ base) (valof ρ step))]
    [`(iter-Nat ,tgt ,base ,step)
     (elim-iter-Nat (valof ρ tgt) (valof ρ base) (valof ρ step))]
    [`(rec-Nat ,tgt ,base ,step)
     (elim-rec-Nat (valof ρ tgt) (valof ρ base) (valof ρ step))]
    [`(λ (,x) ,body) #:when (not (reserved-symbol? x))
     (CLOSURE ρ x body)]
    [`(,rator ,rand) (elim-fun (valof ρ rator) (valof ρ rand))]))



;Nat eliminator helpers
(define/spec (elim-which-Nat tgt base step)
  (-> Value? Value? Value? Value?)
  (cond
    [(N-Val? tgt) (N-WHICH-NAT tgt base step)]
    [(ZERO? tgt) base]
    [else (elim-fun step (ADD1-body tgt))]))

(define/spec (elim-iter-Nat tgt base step)
  (-> Value? Value? Value? Value?)
  (cond
    [(N-Val? tgt) (N-ITER-NAT tgt base step)]
    [(ZERO? tgt) base]
    [else (elim-fun step (elim-iter-Nat (ADD1-body tgt) base step))]))

(define/spec (elim-rec-Nat tgt base step)
  (-> Value? Value? Value? Value?)
  (cond
    [(N-Val? tgt) (N-REC-NAT tgt base step)]
    [(ZERO? tgt) base]
    [else (elim-fun (elim-fun step (ADD1-body tgt))
                    (elim-rec-Nat (ADD1-body tgt) base step))]))


;λ (function) elimination
(define/spec (elim-fun rator rand)
  (-> Value? Value? Value?)
  (match rator
    [(CLOSURE ρ id body)
     (valof (extend ρ id rand)
            body)]
    [(? N-Val?) (N-APP rator rand)]))


;;returns a version of id not in used
(define freshen
  (λ (used x)
    (cond
      [(not (member x used)) x]
      [else (freshen-helper used x 1)])))

(define freshen-helper
  (λ (used x n)
    (let ([xn (string->symbol
               (string-append
                (symbol->string x)
                (number->string n)))])
      (if (member xn used)
          (freshen-helper used x (add1 n))
          xn))))

;;tests for freshen
(check-equal? (freshen (list 'x 'y 'y1 'z 'z1 'z2) 'w) 'w)
(check-equal? (freshen (list 'x 'y 'y1 'z 'z1 'z2) 'x) 'x1)
(check-equal? (freshen (list 'x 'y 'y1 'z 'z1 'z2) 'y) 'y2)
(check-equal? (freshen (list 'x 'y 'y1 'z 'z1 'z2) 'z) 'z3)



;; reads val back as an expression
(define/spec (read-back used-names val)
  (-> ListId? (either N-Val? Value?) (either NeutralExpr? NormalExpr?))
  (match val
    [(ZERO) 'zero]
    [(ADD1 body) `(add1 ,(read-back used-names body))]
    [(N-WHICH-NAT tgt base step)
     `(which-Nat ,(read-back used-names tgt) ,(read-back used-names base) ,(read-back used-names step))]
    [(N-ITER-NAT tgt base step)
     `(iter-Nat ,(read-back used-names tgt) ,(read-back used-names base) ,(read-back used-names step))]
    [(N-REC-NAT tgt base step)
     `(rec-Nat ,(read-back used-names tgt) ,(read-back used-names base) ,(read-back used-names step))]
    [(CLOSURE ρ id body)
     (let ([fresh-id (freshen used-names id)])
       `(λ (,fresh-id)
          ,(read-back (cons fresh-id used-names)
                      (valof (extend ρ id (N-ID fresh-id))
                             body))))]
    [(N-ID x) x]
    [(N-APP rator rand)
     `(,(read-back used-names rator)
       ,(read-back used-names rand))]))


;; normalize an expression by evaluating
;; it and reading back the value as an Expr
(define/spec (normalize ρ e)
  (-> ValueEnv? Expr? NormalExpr?)
  (read-back '() (valof ρ e)))



;; a Define is a (define Id Expr)
(define (Define? e)
  (match e
    [`(define ,name ,body)
     (and (symbol? name)
          (not (reserved-symbol? name))
          (Expr? body))]
    [_ #f]))
;; a Program is a list of Expr or Define
;; that should end with an Expr.
(define (Program? p)
  (and (list p)
       (andmap (λ (e) (or (Expr? e) (Define? e)))
               p)))



;; Evaluates a Program, i.e. for each (define id body),
;; evaluate body in the current value environment ρ to some value v,
;; and then proceeds to evaluate the rest of the program with value
;; environment ρ[id ↦ v]. valof-program returns the list of results
;; from evaluating top level expressions.
(define/spec (valof-program ρ program)
  (-> ValueEnv? Program? (listof NormalExpr?))
  (match program
    ;; this match pattern matches a cons where the
    ;; car matches the pattern `(define ,x ,e)
    ;; and the cdr can be anything (and is bound
    ;; to the identifier rest-of-the-program)
    [`((define ,x ,e) . ,rest-of-the-program)
     (valof-program (extend ρ x (valof ρ e))
                    rest-of-the-program)]
    ;; this match pattern is like the above one (i.e. a cons)
    ;; but only succeeds when the #:when predicate (Expr? e)
    ;; is also non-#f
    [`(,e . ,rest-of-the-program)
     #:when (Expr? e)
     (cons (normalize ρ e)
           (valof-program ρ rest-of-the-program))]
    ['() '()]))



;; given an expression e, returns a program
;; that begins with the following definitions
;; - church-zero
;; - church-add1
;; - church-plus
;; and that ends with expression e.
;; NOTE: this allows us to write simple programs
;; that use church-numerals in them, which can be
;; useful for testing a bare-bones language like
;; the λ-calculus.
(define/spec (with-church-numerals e)
  (-> Expr? Program?)
  `((define church-zero
      (λ (f) (λ (x) x)))
    (define church-add1
      (λ (n-1) (λ (f)
                 (λ (x)
                   (f ((n-1 f) x))))))
    (define church-plus
      (λ (j)
        (λ (k) (λ (f)
                 (λ (x)
                   ((j f) ((k f) x)))))))
    ,e))


;; converts a natural number (e.g. 2) to a church
;; numeral of equal value (e.g. (λ (f) (λ (x) (f (f x)))))
(define/spec (to-church n)
  (-> exact-nonnegative-integer? Expr?)
  (cond [(zero? n) 'church-zero]
        [else
         (let ([church-of-n-1 (to-church (sub1 n))])
           `(church-add1 ,church-of-n-1))]))


;;tests for ADD1 struct
(check-equal? (ADD1-body (ADD1 (ZERO)))
              (ZERO))
(check-equal? (ADD1-body (ADD1 (ADD1 (ZERO))))
              (ADD1 (ZERO)))
(check-equal? (ADD1 (ADD1-body (ADD1 (ADD1 (ZERO)))))
              (ADD1 (ADD1 (ZERO))))


;;tests for Value?
(check-equal? (Value? (CLOSURE '() 'x `(λ (x) x)))
              #t)
(check-equal? (Value? (ZERO))
              #t)
(check-equal? (Value? (ADD1 (ZERO)))
              #t)


;;tests for Expr?
(check-equal? (Expr? '(λ (x) x))
              #t)
(check-equal? (Expr? 'c)
              #t)
(check-equal? (Expr? `((λ (x) x)
                       'c))
              #t)
(check-equal? (Expr? `(add1 zero))
              #t)
(check-equal? (Expr? `(λ (x) (add1 zero)))
              #t)

(check-equal? (Expr? `(which-Nat (add1 zero)
                                 zero
                                 (λ (x) zero)))
              #t)
(check-equal? (Expr? `(iter-Nat (add1 zero)
                                 zero
                                 (λ (x) zero)))
              #t)
(check-equal? (Expr? `(rec-Nat (add1 zero)
                                 zero
                                 (λ (x) zero)))
              #t)


;; tests
(check-equal?
 (valof-program
  '()
  '((λ (x) (λ (x) (λ (y) (y x))))))
 '((λ (x) (λ (x1) (λ (y) (y x1))))))
(check-equal?
 (valof-program
  '()
  '((λ (f) (λ (x) ((f x) f)))))
 '((λ (f) (λ (x) ((f x) f)))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (λ (y) (λ (z) (z y))))))
 '((λ (y) (λ (z) (z y)))))
(check-equal?
 (valof-program
  '()
  (with-church-numerals (to-church 0)))
 '((λ (f) (λ (x) x))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 1)))
 '((λ (f) (λ (x) (f x)))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 4)))
 '((λ (f) (λ (x) (f (f (f (f x))))))))
(check-equal?
 (valof-program
  '()
  (with-church-numerals
      `((church-plus ,(to-church 2)) ,(to-church 2))))
 '((λ (f) (λ (x) (f (f (f (f x))))))))
(check-equal?
 (valof-program
  '()
  (with-church-numerals
      `((church-plus
         ((church-plus
           ,(to-church 1))
          ,(to-church 2)))
        ,(to-church 3))))
 '((λ (f) (λ (x) (f (f (f (f (f (f x))))))))))
(check-equal?
 (valof-program
  '()
  '((λ (x) (λ (x) (λ (y) (y x))))))
 '((λ (x) (λ (x1) (λ (y) (y x1))))))
(check-equal?
 (valof-program
  '()
  '((which-Nat zero (add1 zero) (λ (x) x))))
 '((add1 zero)))

(check-equal?
 (valof-program
  '()
  '((iter-Nat (add1 (add1 zero))
              zero
              (λ (val-n-1)
                (add1 (add1 val-n-1))))))
 '((add1 (add1 (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '((λ (n)
      (which-Nat n
                 zero
                 (λ (x) x)))))
 '((λ (n) (which-Nat n
                     zero
                     (λ (x) x)))))
(check-equal?
 (valof-program
  '()
  '((λ (n)
      (which-Nat n
                 zero
                 (λ (x) zero)))))
 '((λ (n) (which-Nat n
                     zero
                     (λ (x) zero)))))
(check-equal?
 (valof-program
  '()
  '(((λ (x)
       (iter-Nat x
                 zero
                 (λ (val-1)
                   (add1 x))))
     (add1 (add1 (add1 zero))))))
 '((add1 (add1 (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '(((λ (x)
       (iter-Nat x
                 zero
                 (λ (val-1)
                   (add1 val-1))))
     (add1 (add1 (add1 zero))))))
 '((add1 (add1 (add1 zero)))))
;; start of better tests
(check-equal?
 (valof-program
   '()
   '((λ (x) (add1 x))))
 '((λ (x) (add1 x))))
(check-equal?
 (valof-program
  '()
  '((which-Nat zero
               zero
               (λ (x) x))))
 '(zero))
(check-equal?
 (valof-program
  '()
  '((λ (n) (which-Nat zero
                      n
                      (λ (x) x)))))
 '((λ (n) n)))
(check-equal?
 (valof-program
  '()
  '((which-Nat (add1 (add1 zero))
               zero
               (λ (x)
                 (add1 (add1 x))))))
 '((add1 (add1 (add1 zero)))))
(check-equal?
 (valof-program
  '()
  '((λ (n) (which-Nat n
                      zero
                      (λ (x) (add1 x))))))
 '((λ (n) (which-Nat n
                     zero
                     (λ (x) (add1 x))))))
(check-equal?
 (valof-program
  '()
  '((iter-Nat zero
              (add1 zero)
              (λ (x) x))))
 '((add1 zero)))
(check-equal?
 (valof-program
  '()
  '((λ (n) (iter-Nat zero
                     n
                     (λ (x) x)))))
 '((λ (n) n)))
(check-equal?
 (valof-program
  '()
  '((iter-Nat (add1 (add1 zero))
              zero
              (λ (x)
                (add1 (add1 x))))))
 '((add1 (add1 (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '((λ (n) (iter-Nat n
                     zero
                     (λ (x) (add1 x))))))
 '((λ (n) (iter-Nat n
                    zero
                    (λ (x) (add1 x))))))
(check-equal?
 (valof-program
  '()
  '(((λ (n) (iter-Nat n
                      (add1 n)
                      (λ (x) (add1 n))))
     (add1 (add1 zero)))))
 '((add1 (add1 (add1 zero)))))
(check-equal?
 (valof-program
  '()
  '((rec-Nat zero
             (add1 zero)
             (λ (x)
               (λ (y)
                 x)))))
 '((add1 zero)))
(check-equal?
 (valof-program
  '()
  '((rec-Nat (add1 (add1 zero))
             zero
             (λ (x)
               (λ (y)
                 (add1 x))))))
 '((add1 (add1 zero))))
(check-equal?
 (valof-program
  '()
  '((rec-Nat (add1 (add1 zero))
             zero
             (λ (x)
               (λ (y)
                 (add1 (add1 y)))))))
 '((add1 (add1 (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '((λ (n) (rec-Nat n
                    zero
                    (λ (x)
                      (λ (y)
                        x))))))
 '((λ (n) (rec-Nat n zero (λ (x) (λ (y) x))))))
(check-equal?
 (valof-program
  '()
  '(((λ (n)
       ((λ (m)
          (rec-Nat n
                   m
                   (λ (cur-1)
                     (λ (m+cur-1)
                       (add1 m+cur-1)))))
        (add1 (add1 zero))))
     (add1 (add1 (add1 zero))))))
 '((add1 (add1 (add1 (add1 (add1 zero)))))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (λ (y) (λ (z) (z y))))))
 '((λ (y) (λ (z) (z y)))))
(check-equal?
 (valof-program
  '()
  '(((λ (x) zero)
     (add1 zero))))
 '(zero))
(check-equal?
 (valof-program
  '()
  '(((λ (x) x)
     zero)))
 '(zero))
(check-equal?
 (valof-program
  '()
  '(((λ (x) x)
     ((λ (x) x)
      zero))))
 '(zero))
(check-equal?
 (valof-program
  '()
  '((λ (y)
      ((λ (x) x)
       ((λ (x) y)
        zero)))))
 '((λ (y) y)))

;;-------------------------------------------------------
;; instructor tests
(check-equal?
 (valof-program
  '()
  '((λ (x) (λ (x) (λ (y) (y x))))))
 '((λ (x) (λ (x1) (λ (y) (y x1))))))
(check-equal?
 (valof-program
  '()
  '((λ (f) (λ (x) ((f x) f)))))
 '((λ (f) (λ (x) ((f x) f)))))
(check-equal? 
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (λ (y) (λ (z) (z y))))))
 '((λ (y) (λ (z) (z y)))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 0)))
 '((λ (f) (λ (x) x))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 1)))
 '((λ (f) (λ (x) (f x)))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 4)))
 '((λ (f) (λ (x) (f (f (f (f x))))))))
(check-equal?
 (valof-program
  '()
  (with-church-numerals
      `((church-plus ,(to-church 2)) ,(to-church 2))))
 '((λ (f) (λ (x) (f (f (f (f x))))))))
(check-equal?
 (valof-program
  '()
  (with-church-numerals
      `((church-plus
         ((church-plus
           ,(to-church 1))
          ,(to-church 2)))
        ,(to-church 3))))
 '((λ (f) (λ (x) (f (f (f (f (f (f x))))))))))


;; basic nat tests
;;
;; zero
(check-equal?
 (valof-program
  '()
  '(zero))
 '(zero))
;; add1
(check-equal?
 (valof-program
  '()
  '((add1 zero)))
 '((add1 zero)))
;; which-Nat of zero
(check-equal?
 (valof-program
  '()
  '((which-Nat zero (add1 zero) (λ (x) x))))
 '((add1 zero)))
;; which-Nat of add1
(check-equal?
 (valof-program
  '()
  '((which-Nat (add1 (add1 zero)) zero (λ (x) x))))
 '((add1 zero)))
;; iter-Nat of zero
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ zero) (add1 zero))))
 '((add1 zero)))
;; iter-Nat of add1
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ (add1 zero)) (add1 zero))))
 '((add1 (add1 zero))))
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ (add1 zero)) (add1 (add1 zero)))))
 '((add1 (add1 (add1 zero)))))
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ (add1 (add1 zero))) (add1 zero))))
 '((add1 (add1 (add1 zero)))))
;; rec-Nat of zero
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    ((+ zero) (add1 zero))))
 '((add1 zero)))
;; rec-Nat of add1
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    ((+ (add1 (add1 zero))) (add1 zero))))
 '((add1 (add1 (add1 zero)))))
;; which-Nat of zero under a λ
(check-equal?
 (valof-program
  '()
  '((define foo (λ (n) (λ (m) (which-Nat n m (λ (x) (add1 x))))))
    (foo zero)))
 '((λ (m) m)))
;; which-Nat of add1 under λ
(check-equal?
 (valof-program
  '()
  '((define bar (λ (n) (λ (m) (which-Nat n m (λ (x) (add1 x))))))
    (bar (add1 zero))))
 '((λ (m) (add1 zero))))
;; iter-Nat of zero under a λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    (+ zero)))
 '((λ (m) m)))
;; iter-Nat of add1 under λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    (+ (add1 zero))))
 '((λ (m) (add1 m))))
;; rec-Nat of zero under a λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    (+ zero)))
 '((λ (m) m)))
;; rec-Nat of add1 under λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    (+ (add1 zero))))
 '((λ (m) (add1 m))))
;; neutral which-Nat
(check-equal?
 (valof-program
  '()
  '((define foo (λ (n) (λ (m) (which-Nat n m (λ (x) (add1 x))))))
    (λ (z) ((foo z) zero))))
 '((λ (z) (which-Nat z zero (λ (x) (add1 x))))))
;; neutral iter-Nat
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    (λ (z) ((+ z) zero))))
 '((λ (z) (iter-Nat z zero (λ (x) (add1 x))))))
;; neutral rec-Nat
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    (λ (z) ((+ z) zero))))
 '((λ (z) (rec-Nat z zero (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
