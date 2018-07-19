#lang racket/base

(require racket/match
         racket/list
         define-with-spec
         rackunit)

;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Assignment "lang2"
;; due 30 Jan by 3pm
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(define Id? symbol?)


(define/spec (reserved-symbol? x)
  (-> symbol? boolean?)
  (and (member x '(λ zero add1 which-Nat iter-Nat rec-Nat)) #t))

;; a predicate for Exprs
;; Expr ::= Id | (λ (Id) Expr) | (Expr Expr)
(define (Expr? e)
  (match e
    ;; The match pattern (? symbol? x) will succeed for
    ;; any value for which symbol? returns #t, and if
    ;; so it will bind that value to x in the rhs.
    ;; 'add1 and 'zero will successfully return #t
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
    ;; the match pattern `(λ (,x) ,body) will match any
    ;; list of length 3 where:
    ;; - the first element is the symbol 'λ
    ;; - the second element is a list with one element
    ;;   (and that one element is bound to "x" in the rhs)
    ;; - the third element can be anything (and is bound to "body")
    [`(λ (,x) ,body)
     (and (symbol? x)
          (not (reserved-symbol? x))
          (Expr? body))]
    ;; the match pattern `(,rator ,rand) matches any list
    ;; of length two and binds the first element to rator
    ;; and the second element to rand in the rhs.
    [`(,rator ,rand)
     (and (Expr? rator)
          (Expr? rand))]
    [_ #f]))

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
                                 ADD1?)]))

(define NUMBER? (λ (n) (or (ADD1? n)
                           (ZERO? n))))

;; a Value is a CLOSURE
(define Value? (λ (x)
                 (or (NUMBER? x)
                     (CLOSURE? x))))




;; a ValueEnv is a (listof (list Id? Value?))
(define (ValueEnv? ρ)
  (and (list? ρ)
       (andmap (λ (entry) (and (= 2 (length entry))
                               (Id? (first entry))
                               (Value? (second entry))))
               ρ)))


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

;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Exercise 1
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Design and test a function
;;   valof : ValueEnv Expr -> Value
;; that reduces a Expr e to a Value in ValueEnv ρ.
;;
;; When we say "test", we mean you should write
;; "check-equal?" statements after the definition that
;; demonstrate that valof is correct.
;;
;; NOTE: For this course, testing is an important part of
;; showing that you understand your solution.
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
  (if (ZERO? tgt)
      base
      (elim-fun step (ADD1-body tgt))))

(define/spec (elim-iter-Nat tgt base step)
  (-> Value? Value? Value? Value?)
  (if (ZERO? tgt)
      base
      (elim-fun step (elim-iter-Nat (ADD1-body tgt) base step))))

(define/spec (elim-rec-Nat tgt base step)
  (-> Value? Value? Value? Value?)
  (if (ZERO? tgt)
      base
      (elim-fun (elim-fun step (ADD1-body tgt))
                (elim-rec-Nat (ADD1-body tgt) base step))))


;λ (function) elimination
(define/spec (elim-fun rator rand)
  (-> Value? Value? Value?)
  (match rator
    [(CLOSURE ρ id body)
     (valof (extend ρ id rand)
            body)]))

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
  (-> ValueEnv? Program? (listof Value?))
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
     (cons (valof ρ e)
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



;;my tests for nats/nat eliminators
(check-equal?
 (valof-program
  '()
  '(zero))
 (list (ZERO)))
(check-equal?
 (valof-program
  '()
  '((add1 (add1 zero))))
 (list (ADD1 (ADD1 (ZERO)))))
(check-equal?
 (valof-program
  '()
  '((which-Nat zero
               (add1 zero)
               (λ (x) x))))
 (list (ADD1 (ZERO))))
(check-equal?
 (valof-program
  '()
  '((which-Nat (add1 zero)
               (add1 zero)
               (λ (x) x))))
 (list (ZERO)))
(check-equal?
 (valof-program
  '()
  '((define step-+
      (λ (+n-1)
        (add1 +n-1)))
    (iter-Nat (add1 (add1 zero))
              (add1 (add1 (add1 zero)))
              step-+)))
 (list (ADD1 (ADD1 (ADD1 (ADD1 (ADD1 (ZERO))))))))
(check-equal?
 (valof-program
  '()
  '((rec-Nat (add1 (add1 zero))
             zero
             (λ (n-1)
               (λ (rec-n-1)
                 (add1 rec-n-1))))))
 (list (ADD1 (ADD1 (ZERO)))))
(check-equal?
 (valof-program
  '()
  '((rec-Nat (add1 (add1 zero))
             zero
             (λ (n-1)
               (λ (rec-n-1)
                 (add1 n-1))))))
 (list (ADD1 (ADD1 (ZERO)))))
(check-equal? (valof-program
               '()
               '(((λ (n)
                    (λ (m)
                      (rec-Nat n
                               m
                               (λ (cur-1)
                                 (λ (m+cur-1)
                                   (add1 m+cur-1))))))
                  (add1 (add1 (add1 zero))))
                 (add1 (add1 zero))))
              (list
               (CLOSURE
                (list (list 'n (ADD1 (ADD1 (ADD1 (ZERO))))))
                'm
                '(rec-Nat n m (λ (cur-1) (λ (m+cur-1) (add1 m+cur-1)))))
               (ADD1 (ADD1 (ZERO)))))
(check-equal?
 (valof-program
  '()
  '((λ (n)
      (which-Nat n
                 zero
                 (λ (x) x)))))
 (list
  (CLOSURE
   '()
   'n
   '(which-Nat n zero (λ (x) x)))))
(check-equal?
 (valof-program
  '()
  '((iter-Nat zero
              zero
              (λ (x) (add1 x)))))
 (list (ZERO)))
(check-equal?
 (valof-program
  '()
  '((rec-Nat zero
             zero
             (λ (x) (λ (y) (add1 x))))))
 (list (ZERO)))
(check-equal?
 (valof-program
  '()
  '((λ (n) (rec-Nat n
                    zero
                    (λ (x) (λ (y) zero))))))
 (list (CLOSURE '() 'n '(rec-Nat n zero (λ (x) (λ (y) zero))))))
(check-equal?
 (valof-program
  '()
  '((λ (n) (iter-Nat n
                     zero
                     (λ (x) zero)))))
 (list (CLOSURE '() 'n '(iter-Nat n zero (λ (x) zero)))))

;; tests for valof/valof-program... note how awful
;; explicitly dealing with closures is (especially
;; when we're checking value equality!)
(check-equal?
 (valof-program
  '()
  '((λ (x) (λ (x) (λ (y) (y x))))))
 (list (CLOSURE '() 'x '(λ (x) (λ (y) (y x))))))
(check-equal?
 (valof-program
  '()
  '((define id (λ (x) x))
    (id (λ (y) (λ (z) (z y))))))
 (list (CLOSURE (list (list 'id (CLOSURE '() 'x 'x))) 'y '(λ (z) (z y)))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 0)))
 (list (CLOSURE '() 'f '(λ (x) x))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 1)))
 (list
  (CLOSURE
   `((n-1 ,(CLOSURE '() 'f '(λ (x) x)))
     (church-zero ,(CLOSURE '() 'f '(λ (x) x))))
   'f
   '(λ (x) (f ((n-1 f) x))))))
(check-equal?
 (valof-program '() (with-church-numerals (to-church 4)))
 (list
  (CLOSURE
   (list
    (list
     'n-1
     (CLOSURE
      (list
       (list
        'n-1
        (CLOSURE
         (list
          (list
           'n-1
           (CLOSURE
            (list
             (list
              'n-1
              (CLOSURE '() 'f '(λ (x) x)))
             (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
            'f
            '(λ (x) (f ((n-1 f) x)))))
          (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
         'f '(λ (x) (f ((n-1 f) x)))))
       (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
      'f
      '(λ (x) (f ((n-1 f) x)))))
    (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
   'f
   '(λ (x) (f ((n-1 f) x))))))

(check-equal?
 (valof-program '()
  (with-church-numerals
      `((church-plus ,(to-church 2)) ,(to-church 2))))
 (list
  (CLOSURE
   (list
    (list
     'k
     (CLOSURE
      (list
       (list
        'n-1
        (CLOSURE
         (list
          (list
           'n-1
           (CLOSURE '() 'f '(λ (x) x)))
          (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
         'f
         '(λ (x) (f ((n-1 f) x)))))
       (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
      'f
      '(λ (x) (f ((n-1 f) x)))))
    (list
     'j
     (CLOSURE
      (list
       (list
        'n-1
        (CLOSURE
         (list
          (list
           'n-1
           (CLOSURE '() 'f '(λ (x) x)))
          (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
         'f
         '(λ (x) (f ((n-1 f) x)))))
       (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
      'f
      '(λ (x) (f ((n-1 f) x)))))
    (list
     'church-add1
     (CLOSURE
      (list
       (list
        'church-zero
        (CLOSURE '() 'f '(λ (x) x))))
      'n-1
      '(λ (f) (λ (x) (f ((n-1 f) x))))))
    (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
   'f
   '(λ (x) ((j f) ((k f) x))))))

(check-equal?
 (valof-program
  '()
  (with-church-numerals
      `((church-plus
         ((church-plus
           ,(to-church 1))
          ,(to-church 2)))
        ,(to-church 3))))
 (list
  (CLOSURE
   (list
    (list
     'k
     (CLOSURE
      (list
       (list
        'n-1
        (CLOSURE
         (list
          (list
           'n-1
           (CLOSURE
            (list
             (list 'n-1 (CLOSURE '() 'f '(λ (x) x)))
             (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
            'f
            '(λ (x) (f ((n-1 f) x)))))
          (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
         'f
         '(λ (x) (f ((n-1 f) x)))))
       (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
      'f
      '(λ (x) (f ((n-1 f) x)))))
    (list
     'j
     (CLOSURE
      (list
       (list
        'k
        (CLOSURE
         (list
          (list
           'n-1
           (CLOSURE
            (list
             (list 'n-1 (CLOSURE '() 'f '(λ (x) x)))
             (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
            'f
            '(λ (x) (f ((n-1 f) x)))))
          (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
         'f
         '(λ (x) (f ((n-1 f) x)))))
       (list
        'j
        (CLOSURE
         (list
          (list 'n-1 (CLOSURE '() 'f '(λ (x) x)))
          (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
         'f
         '(λ (x) (f ((n-1 f) x)))))
       (list
        'church-add1
        (CLOSURE
         (list (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
         'n-1
         '(λ (f) (λ (x) (f ((n-1 f) x))))))
       (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
      'f
      '(λ (x) ((j f) ((k f) x)))))
    (list
     'church-add1
     (CLOSURE
      (list (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
      'n-1
      '(λ (f) (λ (x) (f ((n-1 f) x))))))
    (list 'church-zero (CLOSURE '() 'f '(λ (x) x))))
   'f
   '(λ (x) ((j f) ((k f) x))))))

