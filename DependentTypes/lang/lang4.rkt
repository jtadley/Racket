#lang racket/base


;; Assignment lang4
;; Due: Fri 30 March at 5pm



;; Description:
;; In this assignment we get a little experience working with the "locally-nameless"
;; approach to reasoning about bindings by (more or less) tweaking our lang2 interpreter
;; to use it instead of closures.
;;
;; An excellent resource for the locally-nameless approach is the following paper:
;; "I am not a number, I am a free variable" by Conor McBride & James McKinna.



;; Instructions:
;; 0. Familiarize yourself with the code in this file.
;; 1. Replace all `(error "TODO")` with code so that all of the tests
;;    at the end of the file pass.
;; 2. While completing (1), write tests for abstract + instantiate
;;    so you (and the graders) are confident they are each correct.


;; IMPORTANT NOTE: the only functions which manually match on and walk under
;; a Scope to perform some computation should be `instantiate` and `abstract`
;; -- all other functions which need to operate under a λ should use
;; instantiate (e.g. to remove the scope) and abstract (e.g. to re-apply the scope)

(require define-with-spec
         racket/match
         racket/list
         racket/function
         rackunit)


(define id? symbol?)

(define (var? x)
  (and (id? x) (not (memq x '(scope λ zero add1 which-Nat iter-Nat rec-Nat)))))
(define nat? exact-nonnegative-integer?)

;; Id ::= something that id? returns #true for
;; Var ::= something that var? returns #true for
;; Both of the above represent a free identifier in the current AST.

;; surface-language expressions
;; (i.e. the syntax the programs are written in)
;; SExpr ::= Id | (λ (Var) SExpr) | (SExpr SExpr)
;;        | zero | (add1 SExpr) | (which-Nat SExpr SExpr SExpr)
;;        | (iter-Nat SExpr SExpr SExpr) | (rec-Nat SExpr SExpr SExpr)
(define/spec (surface-expr? e)
  (-> any boolean?)
  (match e
    [(? var?) #t]
    [`(λ (,(? var?)) ,(? surface-expr?)) #t]
    [`(,(? surface-expr?) ,(? surface-expr?)) #t]
    ['zero #t]
    [`(add1 ,(? surface-expr?)) #t]
    [`(,nat-elim ,(? surface-expr?) ,(? surface-expr?) ,(? surface-expr?))
     (and (memq nat-elim '(which-Nat iter-Nat rec-Nat)) #t)]
    [_ #f]))


;; Nat ::= A natural number (i.e. a bound identifier in the current AST)


;; locally nameless expressions
;; (i.e. the internal representation that we work with)
;; LNExpr ::= Id | Nat | (λ Scope) | (LNExpr LNExpr)
;;        | zero | (add1 LNExpr) | (which-Nat LNExpr LNExpr LNExpr)
;;        | (iter-Nat LNExpr LNExpr LNExpr) | (rec-Nat LNExpr LNExpr LNExpr)
(define/spec (ln-expr? e)
  (-> any boolean?)
  (match e
    [(? var?) #t]
    [(? nat?) #t]
    [`(λ ,(? scope?)) #t]
    [`(,(? ln-expr?) ,(? ln-expr?)) #t]
    ['zero #t]
    [`(add1 ,(? ln-expr?)) #t]
    [`(,nat-elim ,(? ln-expr?) ,(? ln-expr?) ,(? ln-expr?))
     (and (memq nat-elim '(which-Nat iter-Nat rec-Nat)) #t)]
    [_ #f]))

;; normal values (i.e. a subset of LNExpr)
;; NormalVal ::= zero | (add1 NeutralLNExpr) | (λ (scope NormalLNExpr))
(define/spec (normal-value? x)
  (-> any boolean?)
  (match x
    ['zero #t]
    [`(add1 ,n) (neutral-ln-expr? n)]
    [`(λ (scope ,body)) (normal-ln-expr? body)]
    [_ #f]))

;; normal form LNExprs
;; NormalLNExpr ::= NormalVal | NeutralLNExpr
(define/spec (normal-ln-expr? e)
  (-> any boolean?)
  (or (normal-value? e)
      (neutral-ln-expr? e)))

;; neutral LNExprs
;; NeutralLNExpr ::= Nat
;;                | (NeutralLNExpr NormalLNExpr)
;;                | (which-Nat NeutralLNExpr NormalLNExpr NormalLNExpr NormalLNExpr)
;;                | (iter-Nat  NeutralLNExpr NormalLNExpr NormalLNExpr NormalLNExpr)
;;                | (rec-Nat   NeutralLNExpr NormalLNExpr NormalLNExpr NormalLNExpr)
(define/spec (neutral-ln-expr? e)
  (-> any boolean?)
  (match e
    [(? var?) #t] ;;added
    [(? nat?) #t]
    [`(,(? neutral-ln-expr?) ,(? normal-ln-expr?)) #t]
    [`(,nat-elim ,(? neutral-ln-expr?) ,(? normal-ln-expr?) ,(? normal-ln-expr?))
     (and (memq nat-elim '(which-Nat iter-Nat rec-Nat)) #t)]
    [_ #f]))


;; a scope is just a syntactic indication that an expression
;; is under a scope/binding -- this helps us make sure we've
;; abstracted/instantiated as we're doing transformations
;;
;; Scope ::= (scope LNExpr)
(define/spec (scope? x)
  (-> any boolean?)
  (match x
    [`(scope ,ln-expr?) #t]
    [_ #f]))



;; a env is a mapping from id to Normal Exprs
(define (env? l)
  (and (list? l)
       (andmap
        (match-lambda
          [(list (? var?) (? ln-expr?)) #t]
          [_ #f])
        l)))

;; lookup the value of x in ρ
(define/spec (lookup ρ x)
  (-> env? var? (either ln-expr? #f))
  (match (assoc x ρ)
    [#f #f]
    [(list _ val) val]))

;; extend ρ with the mapping from x to v
(define/spec (extend ρ x v)
  (-> env? var? ln-expr? env?)
  (cons (list x v) ρ))

(define/spec (find-free-var ρ)
  (-> env? var?)
  (let ([free-var (gensym)])
    (if (lookup ρ free-var)
        (find-free-var ρ)
        free-var)))

; * * * * * * * * * * * * * * * * *
; normalize (i.e. acts sort of like `valof`
; from assignment lang2 except that it just
; uses s-expressions and no explicit value
; structs during computation/normalization)
; * * * * * * * * * * * * * * * * *
(define/spec (normalize ρ e)
  (-> env? ln-expr? ln-expr?)
  (match e
    ['zero 'zero]
    [`(add1 ,(? ln-expr? body)) `(add1 ,(normalize ρ body))]
    [(? var? y) (or (lookup ρ y)
                    (error 'normalize "unbound identifier ~v" y))]
    [(? nat? n) n]
    [`(λ ,(? scope? body))
     (let ([free-var (find-free-var ρ)])
       `(λ ,(abstract (normalize (extend ρ free-var free-var) (instantiate body free-var)) free-var)))]
    [`(,(? ln-expr? rator) ,(? ln-expr? rand)) (elim-fun ρ (normalize ρ rator) (normalize ρ rand))]
    [`(,nat-elim ,(? ln-expr? tgt) ,(? ln-expr? base) ,(? ln-expr? step))
     (elim-nat-elim ρ nat-elim (normalize ρ tgt) (normalize ρ base) (normalize ρ step))]))

(define (nat-elim? x)
  (memq x '(which-Nat iter-Nat rec-Nat)))

(define/spec (elim-nat-elim ρ nat-elim tgt base step)
  (-> env? symbol? ln-expr? ln-expr? ln-expr? ln-expr?)
  (match nat-elim
    ['which-Nat (match tgt
                  [(? neutral-ln-expr?) `(which-Nat ,tgt ,base ,step)]
                  ['zero base]
                  [`(add1 ,body) (elim-fun ρ step body)])]
    ['iter-Nat (match tgt
                 [(? neutral-ln-expr?) `(iter-Nat ,tgt ,base ,step)]
                 ['zero base]
                 [`(add1 ,body) (elim-fun ρ step (elim-nat-elim ρ 'iter-Nat body base step))])]
    ['rec-Nat (match tgt
                [(? neutral-ln-expr?) `(rec-Nat ,tgt ,base ,step)]
                ['zero base]
                [`(add1 ,body) (elim-fun ρ
                                         (elim-fun ρ step body)
                                         (elim-nat-elim ρ 'rec-Nat body base step))])]))

(define/spec (elim-fun ρ rator rand)
  (-> env? ln-expr? ln-expr? ln-expr?)
  (match rator
    [(? neutral-ln-expr?) `(,rator ,rand)]
    [`(λ ,(? scope? body))
     (normalize ρ (instantiate body rand))]))


; * * * * * * * * * * * * * * * * *
; instantiate
; unwraps the scope s, returning the
; underlying expression with DeBruijn
; index 0 (or its successor under a
; binder of course) replaced with e.
;
; e.g. (instantiate '(scope (0 0)) 'zero)
;      ==>
;      '(zero zero)
; * * * * * * * * * * * * * * * * *
(define/spec (instantiate s val)
  (-> scope? ln-expr? ln-expr?)
  (let instantiate ([e s]
                    [depth 0])
    (match e
      [`(scope ,body) (instantiate body depth)]
      ['zero 'zero]
      [`(add1 ,body) `(add1 ,(instantiate body depth))]
      [(? var? x) x]
      [(? nat? n) (if (= n depth) val n)]
      [`(λ (scope ,body)) `(λ (scope ,(instantiate body (add1 depth))))]
      [`(,e1 ,e2) (list (instantiate e1 depth)
                        (instantiate e2 depth))]
      [`(,nat-elim ,tgt ,base ,step)
       #:when (memq nat-elim '(which-Nat iter-Nat rec-Nat))
       `(,nat-elim ,(instantiate tgt depth) ,(instantiate base depth) ,(instantiate step depth))])))


;; INSTANTIATE TESTS
(check-equal?
 (instantiate '(scope (0 0)) 'zero)
 '(zero zero))
(check-equal?
 (instantiate '(scope (x x)) 'y)
 '(x x))
(check-equal?
 (instantiate '(scope (λ (scope (0 0)))) 'y)
 '(λ (scope (0 0))))
(check-equal?
 (instantiate '(scope (λ (scope 0))) 'y)
 '(λ (scope 0)))
(check-equal?
 (instantiate '(scope (λ (scope 1))) 'y)
 '(λ (scope y)))
(check-equal?
 (instantiate '(scope ((λ (scope 1)) 0)) 'y)
 '((λ (scope y)) y))
(check-equal?
 (instantiate '(scope (λ (scope (λ (scope (0 (1 (2 2)))))))) 'y)
 '(λ (scope (λ (scope (0 (1 (y y))))))))
(check-equal?
 (instantiate '(scope ((λ (scope (0 0)))
                       (λ (scope (0 0)))))
   'y)
 '((λ (scope (0 0)))
   (λ (scope (0 0)))))
(check-equal?
 (instantiate '(scope ((λ (scope (1 0)))
                       (λ (scope (0 1)))))
   'y)
 '((λ (scope (y 0)))
   (λ (scope (0 y)))))
(check-equal?
 (instantiate '(scope (which-Nat 0
                                 0
                                 (λ (scope 1))))
   'y)
 '(which-Nat y
             y
             (λ (scope y))))
(check-equal?
 (instantiate '(scope (λ (scope
                          (which-Nat 1
                                     0
                                     (λ (scope (2 0)))))))
   'y)
 '(λ (scope
      (which-Nat y
                 0
                 (λ (scope (y 0)))))))
(check-equal?
 (instantiate '(scope ((λ (scope
                           (which-Nat 0
                                      1
                                      (λ (scope (2 1))))))
                       0))
   'y)
 '((λ (scope (which-Nat 0
                        y
                        (λ (scope (y 1)))))) y))
(check-equal?
 (instantiate '(scope (which-Nat 0
                                 0
                                 (λ (scope 1))))
   'y)
 '(which-Nat y
             y
             (λ (scope y))))
(check-equal?
 (instantiate '(scope (λ (scope
                          (which-Nat 1
                                     0
                                     (λ (scope (2 0)))))))
   'y)
 '(λ (scope
      (which-Nat y
                 0
                 (λ (scope (y 0)))))))
(check-equal?
 (instantiate '(scope ((λ (scope
                           (which-Nat 0
                                      1
                                      (λ (scope (2 1))))))
                       0))
   'y)
 '((λ (scope (which-Nat 0 y (λ (scope (y 1)))))) y))
(check-equal?
 (instantiate '(scope (iter-Nat 0
                                0
                                (λ (scope 1))))
   'y)
 '(iter-Nat y
            y
            (λ (scope y))))
(check-equal?
 (instantiate '(scope (λ (scope
                          (iter-Nat 1
                                    0
                                    (λ (scope (2 0)))))))
   'y)
 '(λ (scope
      (iter-Nat y
                0
                (λ (scope (y 0)))))))
(check-equal?
 (instantiate '(scope ((λ (scope
                           (iter-Nat 0
                                     1
                                     (λ (scope (2 1))))))
                       0))
   'y)
 '((λ (scope (iter-Nat 0
                       y
                       (λ (scope (y 1)))))) y))
(check-equal?
 (instantiate '(scope (rec-Nat 0
                               0
                               (λ (scope (λ (scope 2))))))
   'y)
 '(rec-Nat y
           y
           (λ (scope (λ (scope y))))))
(check-equal?
 (instantiate '(scope (λ (scope
                          (rec-Nat 1
                                   0
                                   (λ (scope (λ (scope (3 1)))))))))
   'y)
 '(λ (scope
      (rec-Nat y
               0
               (λ (scope (λ (scope (y 1)))))))))
(check-equal?
 (instantiate '(scope ((λ (scope
                           (rec-Nat 0
                                    1
                                    (λ (scope (λ (scope (3 2))))))))
                       0))
   'y)
 '((λ (scope (rec-Nat 0
                      y
                      (λ (scope (λ (scope (y 2)))))))) y))




; * * * * * * * * * * * * * * * * *
; abstract
; abstracts away x in e, returning the
; underlying expression with DeBruijn
; index 0 (or its successor under a
; binder of course) replacing x
; and with a scope wrapped around
; the expression.
;
; e.g. (abstract '(x x) 'x)
;      ==>
;      '(scope (0 0))
; * * * * * * * * * * * * * * * * *
(define/spec (abstract e x)
  (-> ln-expr? var? scope?)
  (list
   'scope
   (let abstract ([e e]
                  [depth 0])
     (match e
       ['zero 'zero]
       [`(add1 ,body) `(add1 ,(abstract body depth))]
       [(? var? y) (if (eq? y x) depth y)]
       [(? nat? n) n]
       [`(λ (scope ,body))
        `(λ (scope ,(abstract body (add1 depth))))]
       [`(,e1 ,e2) (list (abstract e1 depth)
                         (abstract e2 depth))]
       [`(,nat-elim ,tgt ,base ,step)
        #:when (memq nat-elim '(which-Nat iter-Nat rec-Nat))
        `(,nat-elim ,(abstract tgt depth) ,(abstract base depth) ,(abstract step depth))]))))

;; ABSTRACT TESTS
(check-equal?
 (abstract '(x x) 'x)
 '(scope (0 0)))
(check-equal?
 (abstract 'zero 'y)
 '(scope zero))
(check-equal?
 (abstract '(add1 (add1 zero)) 'y)
 '(scope (add1 (add1 zero))))
(check-equal?
 (abstract '(add1 (add1 x)) 'x)
 '(scope (add1 (add1 0))))
(check-equal?
 (abstract '(add1 (add1 x)) 'y)
 '(scope (add1 (add1 x))))
(check-equal?
 (abstract 'x 'y)
 '(scope x))
(check-equal?
 (abstract '(λ (scope (λ (scope (0 (1 (y y))))))) 'y)
 '(scope (λ (scope (λ (scope (0 (1 (2 2)))))))))
(check-equal?
 (abstract '(x x) 'y)
 '(scope (x x)))
(check-equal?
 (abstract '(λ (scope (0 y))) 'y)
 '(scope (λ (scope (0 1)))))
(check-equal?
 (abstract '(λ (scope 0)) 'y)
 '(scope (λ (scope 0))))
(check-equal?
 (abstract '(λ (scope y)) 'y)
 '(scope (λ (scope 1))))
(check-equal?
 (abstract '((λ (scope y)) y) 'y)
 '(scope ((λ (scope 1)) 0)))
(check-equal?
 (abstract '(λ (scope (λ (scope (0 (1 (y y))))))) 'y)
 '(scope (λ (scope (λ (scope (0 (1 (2 2)))))))))
(check-equal?
 (abstract 
  '((λ (scope (0 0)))
    (λ (scope (0 0))))
  'y)
 '(scope ((λ (scope (0 0)))
          (λ (scope (0 0))))))
(check-equal?
 (abstract 
  '((λ (scope (y 0)))
    (λ (scope (0 y))))
  'y)
 '(scope ((λ (scope (1 0)))
          (λ (scope (0 1))))))
(check-equal?
 (abstract 
  '(which-Nat y
              y
              (λ (scope y)))
  'y)
 '(scope (which-Nat 0
                    0
                    (λ (scope 1)))))
(check-equal?
 (abstract 
  '(λ (scope
       (which-Nat y
                  0
                  (λ (scope (y 0))))))
  'y)
 '(scope (λ (scope
             (which-Nat 1
                        0
                        (λ (scope (2 0))))))))
(check-equal?
 (abstract 
  '((λ (scope (which-Nat 0
                         y
                         (λ (scope (y 1)))))) y)
  'y)
 '(scope ((λ (scope
              (which-Nat 0
                         1
                         (λ (scope (2 1))))))
          0)))
(check-equal?
 (abstract 
  '(which-Nat y
              y
              (λ (scope y)))
  'y)
 '(scope (which-Nat 0
                    0
                    (λ (scope 1)))))
(check-equal?
 (abstract 
  '(λ (scope
       (which-Nat y
                  0
                  (λ (scope (y 0))))))
  'y)
 '(scope (λ (scope
             (which-Nat 1
                        0
                        (λ (scope (2 0))))))))
(check-equal?
 (abstract 
  '((λ (scope (which-Nat 0 y (λ (scope (y 1)))))) y)
  'y)
 '(scope ((λ (scope
              (which-Nat 0
                         1
                         (λ (scope (2 1))))))
          0)))
(check-equal?
 (abstract 
  '(iter-Nat y
             y
             (λ (scope y)))
  'y)
 '(scope (iter-Nat 0
                   0
                   (λ (scope 1)))))
(check-equal?
 (abstract 
  '(λ (scope
       (iter-Nat y
                 0
                 (λ (scope (y 0))))))
  'y)
 '(scope (λ (scope
             (iter-Nat 1
                       0
                       (λ (scope (2 0))))))))
(check-equal?
 (abstract 
  '((λ (scope (iter-Nat 0
                        y
                        (λ (scope (y 1)))))) y)
  'y)
 '(scope ((λ (scope
              (iter-Nat 0
                        1
                        (λ (scope (2 1))))))
          0)))
(check-equal?
 (abstract 
  '(rec-Nat y
            y
            (λ (scope (λ (scope y)))))
  'y)
 '(scope (rec-Nat 0
                  0
                  (λ (scope (λ (scope 2)))))))
(check-equal?
 (abstract 
  '(λ (scope
       (rec-Nat y
                0
                (λ (scope (λ (scope (y 1))))))))
  'y)
 '(scope (λ (scope
             (rec-Nat 1
                      0
                      (λ (scope (λ (scope (3 1))))))))))
(check-equal?
 (abstract 
  '((λ (scope (rec-Nat 0
                       y
                       (λ (scope (λ (scope (y 2)))))))) y)
  'y)
 '(scope ((λ (scope
              (rec-Nat 0
                       1
                       (λ (scope (λ (scope (3 2))))))))
          0)))


; parses an expression from the program into an expression
; using the locally nameless representation
(define/spec (parse e)
  (-> surface-expr? ln-expr?)
  (match e
    [(? var? y) y]
    ['zero 'zero]
    [`(add1 ,body) `(add1 ,(parse body))]
    [`(which-Nat ,e1 ,e2 ,e3)
     `(which-Nat ,(parse e1)
                 ,(parse e2)
                 ,(parse e3))]
    [`(iter-Nat ,e1 ,e2 ,e3)
     `(iter-Nat ,(parse e1)
                ,(parse e2)
                ,(parse e3))]
    [`(rec-Nat ,e1 ,e2 ,e3)
     `(rec-Nat ,(parse e1)
               ,(parse e2)
               ,(parse e3))]
    [`(λ (,x) ,body) `(λ ,(abstract (parse body) x))]
    [`(,e1 ,e2) `(,(parse e1) ,(parse e2))]))

(define (surface-define? e)
  (match e
    [`(define ,(? var?) ,(? surface-expr?)) #t]
    [_ #f]))
(define (ln-define? e)
  (match e
    [`(define ,(? var?) ,(? ln-expr?)) #t]
    [_ #f]))

(define (program? p)
  (and (list? p)
       (andmap (disjoin surface-expr? surface-define?) p)))
(define (ln-program? p)
  (and (list? p)
       (andmap (disjoin ln-expr? ln-define?) p)))


(define/spec (parse-expr-or-def x)
  (-> (either surface-expr? surface-define?)
      (either ln-expr? ln-define?))
  (match x
    [`(define ,name ,body) `(define ,name ,(parse body))]
    [e (parse e)]))


(define/spec (parse-program p)
  (-> program? ln-program?)
  (map parse-expr-or-def p))

;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; valof-program
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
(define/spec (valof-program ρ program)
  (-> env? program? (listof ln-expr?))
  (match program
    [(cons `(define ,x ,e) es)
     (valof-program (extend ρ x (normalize ρ (parse e))) es)]
    [(cons e es)
     (cons (normalize ρ (parse e))
           (valof-program ρ es))]
    [(list) '()]))


;; given an expression e, returns a program
;; that begins with the following definitions
;; - church-zero
;; - church-add1
;; - church-plus
;; and that ends with expression e.
(define/spec (with-church-numerals e)
  (-> surface-expr? program?)
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
  (-> nat? surface-expr?)
  (cond [(zero? n) 'church-zero]
        [(positive? n)
         (let ([church-of-n-1 (to-church (sub1 n))])
           `(church-add1 ,church-of-n-1))]))

(check-equal?
 (valof-program
  '()
  '((λ (x) (λ (x) (λ (y) (y x))))))
 (parse-program '((λ (x) (λ (x1) (λ (y) (y x1)))))))
(check-equal?
 (valof-program
  '()
  '((λ (f) (λ (x) ((f x) f)))))
 (parse-program '((λ (f) (λ (x) ((f x) f))))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (λ (y) (λ (z) (z y))))))
 (parse-program '((λ (y) (λ (z) (z y))))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 0)))
 (parse-program '((λ (f) (λ (x) x)))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 1)))
 (parse-program '((λ (f) (λ (x) (f x))))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 4)))
 (parse-program '((λ (f) (λ (x) (f (f (f (f x)))))))))
(check-equal?
 (valof-program
  '()
  (with-church-numerals
      `((church-plus ,(to-church 2)) ,(to-church 2))))
 (parse-program '((λ (f) (λ (x) (f (f (f (f x)))))))))
(check-equal?
 (valof-program
  '()
  (with-church-numerals
      `((church-plus
         ((church-plus
           ,(to-church 1))
          ,(to-church 2)))
        ,(to-church 3))))
 (parse-program '((λ (f) (λ (x) (f (f (f (f (f (f x)))))))))))

;; basic nat tests
;;
;; zero
(check-equal?
 (valof-program
  '()
  '(zero))
 (parse-program '(zero)))
;; add1
(check-equal?
 (valof-program
  '()
  '((add1 zero)))
 (parse-program '((add1 zero))))
;; which-Nat of zero
(check-equal?
 (valof-program
  '()
  '((which-Nat zero (add1 zero) (λ (x) x))))
 (parse-program '((add1 zero))))
;; which-Nat of add1
(check-equal?
 (valof-program
  '()
  '((which-Nat (add1 (add1 zero)) zero (λ (x) x))))
 (parse-program '((add1 zero))))
;; iter-Nat of zero
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ zero) (add1 zero))))
 (parse-program '((add1 zero))))
;; iter-Nat of add1
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ (add1 zero)) (add1 zero))))
 (parse-program '((add1 (add1 zero)))))
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ (add1 zero)) (add1 (add1 zero)))))
 (parse-program '((add1 (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    ((+ (add1 (add1 zero))) (add1 zero))))
 (parse-program '((add1 (add1 (add1 zero))))))
;; rec-Nat of zero
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    ((+ zero) (add1 zero))))
 (parse-program '((add1 zero))))
;; rec-Nat of add1
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    ((+ (add1 (add1 zero))) (add1 zero))))
 (parse-program '((add1 (add1 (add1 zero))))))
;; which-Nat of zero under a λ
(check-equal?
 (valof-program
  '()
  '((define foo (λ (n) (λ (m) (which-Nat n m (λ (x) (add1 x))))))
    (foo zero)))
 (parse-program '((λ (m) m))))
;; which-Nat of add1 under λ
(check-equal?
 (valof-program
  '()
  '((define bar (λ (n) (λ (m) (which-Nat n m (λ (x) (add1 x))))))
    (bar (add1 zero))))
 (parse-program '((λ (m) (add1 zero)))))
;; iter-Nat of zero under a λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    (+ zero)))
 (parse-program '((λ (m) m))))
;; iter-Nat of add1 under λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    (+ (add1 zero))))
 (parse-program '((λ (m) (add1 m)))))
;; rec-Nat of zero under a λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    (+ zero)))
 (parse-program '((λ (m) m))))
;; rec-Nat of add1 under λ
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    (+ (add1 zero))))
 (parse-program '((λ (m) (add1 m)))))
;; neutral which-Nat
(check-equal?
 (valof-program
  '()
  '((define foo (λ (n) (λ (m) (which-Nat n m (λ (x) (add1 x))))))
    (λ (z) ((foo z) zero))))
 (parse-program '((λ (z) (which-Nat z zero (λ (x) (add1 x)))))))
;; neutral iter-Nat
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (iter-Nat n m (λ (x) (add1 x))))))
    (λ (z) ((+ z) zero))))
 (parse-program '((λ (z) (iter-Nat z zero (λ (x) (add1 x)))))))
;; neutral rec-Nat
(check-equal?
 (valof-program
  '()
  '((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
    (λ (z) ((+ z) zero))))
 (parse-program '((λ (z) (rec-Nat z zero (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m))))))))





;; random test helper functions
(define (generate-random-addition depth)
  (cond
    [(<= depth 0) (random 10)]
    [(zero? (random 3)) (random 10)]
    [else (+ (generate-random-addition (sub1 depth))
             (generate-random-addition (sub1 depth)))]))

(define (nat->peano-nat n)
  (cond
    [(<= n 0) 'zero]
    [else `(add1 ,(nat->peano-nat (sub1 n)))]))

(define/spec (sexp->ln-expr s)
  (-> any ln-expr?)
  (match s
    [(? nat? n) (nat->peano-nat n)]
    [`(+ ,(app sexp->ln-expr e1)
         ,(app sexp->ln-expr e2))
     `((+ ,e1) ,e2)]))

(define/spec (sexp->nat s)
  (-> any ln-expr?)
  (match s
    [(? nat? n) n]
    [`(+ ,(app sexp->nat n1)
         ,(app sexp->nat n2))
     (+ n1 n2)]))

;; some random tests
(for* ([i (in-range 1000)])
  (define e (generate-random-addition (remainder i 7)))
  (define ln-expr-e (sexp->ln-expr e))
  (check-equal?
   (valof-program
    '()
    `((define + (λ (n) (λ (m) (rec-Nat n m (λ (cur-1) (λ (cur-1+m) (add1 cur-1+m)))))))
      ,ln-expr-e))
   (list (nat->peano-nat (sexp->nat e)))))




;; ensure programs with unbound variables error
(check-exn
 exn:fail?
 (λ () (valof-program
        '()
        '(x))))
(check-exn
 exn:fail?
 (λ () (valof-program
        '()
        '((λ (x) (x y))))))


;; my tests
;zero
(check-equal?
 (valof-program
  '()
  '(zero))
 (parse-program '(zero)))
;add1
(check-equal?
 (valof-program
  '()
  '((add1 zero)))
 (parse-program '((add1 zero))))
(check-equal?
 (valof-program
  '()
  '((add1 (add1 zero))))
 (parse-program '((add1 (add1 zero)))))
;add1 under a λ
(check-equal?
 (valof-program
  '()
  '((λ (x) (add1 (add1 zero)))))
 (parse-program '((λ (x) (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '((λ (x) (add1 (add1 x)))))

 (parse-program '((λ (x) (add1 (add1 x))))))
;application
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (add1 zero))))
 (parse-program '((add1 zero))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id id)))
 (parse-program '((λ (x) x))))


;;------------------
(check-equal?
 (valof-program
  '()
  '((λ (x) (add1 x))))
 (parse-program '((λ (x) (add1 x)))))

(check-equal?
 (valof-program
  '()
  '((which-Nat zero
               zero
               (λ (x) x))))
 (parse-program '(zero)))
(check-equal?
 (valof-program
  '()
  '((λ (n) (which-Nat zero
                      n
                      (λ (x) x)))))
 (parse-program '((λ (n) n))))
(check-equal?
 (valof-program
  '()
  '((which-Nat (add1 (add1 zero))
               zero
               (λ (x)
                 (add1 (add1 x))))))
 (parse-program '((add1 (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '((λ (n) (which-Nat n
                      zero
                      (λ (x) (add1 x))))))
 (parse-program '((λ (n) (which-Nat n
                                    zero
                                    (λ (x) (add1 x)))))))
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
 (parse-program '((λ (n) n))))
(check-equal?
 (valof-program
  '()
  '((iter-Nat (add1 (add1 zero))
              zero
              (λ (x)
                (add1 (add1 x))))))
 (parse-program '((add1 (add1 (add1 (add1 zero)))))))
(check-equal?
 (valof-program
  '()
  '((λ (n) (iter-Nat n
                     zero
                     (λ (x) (add1 x))))))
 (parse-program '((λ (n) (iter-Nat n
                                   zero
                                   (λ (x) (add1 x)))))))
(check-equal?
 (valof-program
  '()
  '(((λ (n) (iter-Nat n
                      (add1 n)
                      (λ (x) (add1 n))))
     (add1 (add1 zero)))))
 (parse-program '((add1 (add1 (add1 zero))))))
(check-equal?
 (valof-program
  '()
  '((rec-Nat zero
             (add1 zero)
             (λ (x)
               (λ (y)
                 x)))))
 (parse-program '((add1 zero))))
(check-equal?
 (valof-program
  '()
  '((rec-Nat (add1 (add1 zero))
             zero
             (λ (x)
               (λ (y)
                 (add1 x))))))
 (parse-program '((add1 (add1 zero)))))
(check-equal?
 (valof-program
  '()
  '((rec-Nat (add1 (add1 zero))
             zero
             (λ (x)
               (λ (y)
                 (add1 (add1 y)))))))
 (parse-program '((add1 (add1 (add1 (add1 zero)))))))
(check-equal?
 (valof-program
  '()
  '((λ (n) (rec-Nat n
                    zero
                    (λ (x)
                      (λ (y)
                        x))))))
 (parse-program '((λ (n) (rec-Nat n zero (λ (x) (λ (y) x)))))))
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
 (parse-program '((add1 (add1 (add1 (add1 (add1 zero))))))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (λ (y) (λ (z) (z y))))))
 (parse-program '((λ (y) (λ (z) (z y))))))
(check-equal?
 (valof-program
  '()
  '(((λ (x) zero)
     (add1 zero))))
 (parse-program '(zero)))
(check-equal?
 (valof-program
  '()
  '(((λ (x) x)
     zero)))
 (parse-program '(zero)))
(check-equal?
 (valof-program
  '()
  '(((λ (x) x)
     ((λ (x) x)
      zero))))
 (parse-program '(zero)))
(check-equal?
 (valof-program
  '()
  '((λ (y)
      ((λ (x) x)
       ((λ (x) y)
        zero)))))
 (parse-program '((λ (y) y))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (add1 zero))))
 (parse-program '((add1 zero))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (λ (y) (id (add1 zero)))))
 (parse-program '((λ (y) (add1 zero)))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (λ (x) (id (add1 zero)))))
 (parse-program '((λ (x) (add1 zero)))))


;;--------
(define abst/inst-tester
  (λ (expr var)
    (check-equal?
     expr
     (instantiate (abstract expr var) var))))

(abst/inst-tester
 '((λ (scope (rec-Nat 0
                      y
                      (λ (scope (λ (scope (2 y))))))))
   y)
 'y)