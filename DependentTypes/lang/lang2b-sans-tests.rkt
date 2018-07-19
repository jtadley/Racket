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
;; Expr ::= Id | (λ (Id) Expr) | (Expr Expr) | (which-Nat Expr Expr Expr) | (iter-Nat Expr Expr Expr) | (rec-Nat Expr Expr Expr)
(define (Expr? e)
  (match e
    [(? symbol? x) (or (not (reserved-symbol? x))
                       (eqv? 'zero x)
                       (eqv? 'add1 x))]
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
     (and (symbol? x)
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


;; NormalExpr ::= NeutralExpr | (λ (Id) NormalExpr)
(define NormalExpr?
  (λ (x) (or (NeutralExpr? x)
             (match x
               [`(λ ,listId ,n-exp)
                (and (ListId? listId)
                     (NormalExpr? n-exp))]
               [_ #f]))))

;; NeutralExpr ::= Id | (NeutralExpr NormalExpr) | (add1 NeutralExpr) | (which-Nat/iter-Nat/rec-Nat NeutralExpr NormalExpr NormalExpr)
(define NeutralExpr?
  (λ (x) (or (Id? x)
             (match x
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

