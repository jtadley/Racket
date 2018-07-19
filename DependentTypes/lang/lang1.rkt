#lang racket/base

(require racket/match
         racket/list
         define-with-spec
         rackunit)

;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Assignment "lang1"
;; due 16 Jan by 3pm
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Exercise 0
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Log onto the course Piazza page
;;
;; signup link: https://piazza.com/iu/spring2018/csci490629
;;
;; course link: https://piazza.com/iu/spring2018/csci490629/home
;;
;; and add a "followup discussion" to the post titled
;; "lang1 post thread".
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Preface/Prerequisites
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; This assignment should mostly be a review of concepts
;; learned in C311/B521, i.e. you should already be familiar
;; with programs generated by the following grammar:
;;
;; Expr ::= Id | (λ (Id) Expr) | (Expr Expr)
;;
;; You should already know what a closure is, what an
;; environment is, what alpha-equivalence is, etc or
;; at least have seen them before and feel like you can
;; quickly refresh your memory on what they mean.
;;
;; We will not be spending time in this course reviewing
;; these concepts in any appreciable detail.
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *




;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; define-with-spec
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; The program you have been given depends on a package
;; called "define-with-spec", which allows us to give
;; specifications to functions and struct definitions
;; that are enforced at runtime.
;;
;; On Piazza, a tutorial titled "InstallingPackages.pdf"
;; is posted under the "Resources" tab that should be
;; helpful in installing "define-with-spec" (and other
;; packages we may need).
;;
;; Using the "define/spec" form to define functions and
;; structs may help detect bugs early and guide your
;; development as assignments get more complicated.
;;
;; It also helps us see what your programs are doing when
;; we grade your solutions.
;;
;; You do not need to have a spec on every function you
;; define, but it would be a good idea to have them on all
;; important, non-trivial functions -- and you must have
;; then on functions we ask you to define as part of
;; an exercise.
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Church Numerals
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; This assignment has a very simple language, and so to
;; have any tests/examples that are remotely interesting
;; we'll use Church numerals.
;;
;; Church numerals are a way of encoding the natural numbers
;; using only λs:
;;
;; 0 ≡ (λ (f) (λ (x) x))
;; 1 ≡ (λ (f) (λ (x) (f x)))
;; 2 ≡ (λ (f) (λ (x) (f (f x))))
;; etc
;;
;; As can be seen, a Church numeral for a natural number n is a
;; curried function of two arguments that applies the first
;; argument to the second argument n times. You can read more
;; about them here:
;; https://en.wikipedia.org/wiki/Church_encoding#Church_numerals
;;
;; You don't need to have a deep understanding of them for this
;; assignment -- we're just using them to define some non-trivial
;; tests/examples.
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Assignment Summary
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Define an interpreter for the (untyped) λ-calculus
;; that uses closures and environments for evaluation.
;; This assignment should be a quick refresher and we
;; will build on this foundation for future "lang"
;; assignments.
;;
;; Once you have correctly defined your interpreter
;; (exercise 1), the check-equal? tests in this file
;; for "valof" should all pass when you click "Run"
;; in DrRacket.
;;
;; After you have defined the interpreter, exercises 2
;; and 3 will have you define some expressions which
;; also have check-equal? tests that should pass when
;; you are complete.
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


(define Id? symbol?)


(define/spec (reserved-symbol? x)
  (-> symbol? boolean?)
  (and (member x '(λ)) #t))

;; a predicate for Exprs
;; Expr ::= Id | (λ (Id) Expr) | (Expr Expr)
(define (Expr? e)
  (match e
    ;; The match pattern (? symbol? x) will succeed for
    ;; any value for which symbol? returns #t, and if
    ;; so it will bind that value to x in the rhs.
    [(? symbol? x) (not (reserved-symbol? x))]
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


;; a Value is a CLOSURE
(define Value? CLOSURE?)

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
    [`,y #:when (Id? y) (lookup ρ y)]
    [`(λ (,x) ,body)
     (CLOSURE ρ x body)]
    [`(,rator ,rand) (let ([rator-clos (valof ρ rator)]
                           [rand-clos (valof ρ rand)])
                       (valof (extend (CLOSURE-ρ rator-clos) (CLOSURE-id rator-clos) rand-clos) (CLOSURE-body rator-clos)))]))



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



;; Decides if two values are equivalent up
;; to consistent renaming.
(define/spec (α/value=? val1 val2)
  (-> Value? Value? boolean?)
  (value-equiv? val1 val2 (list) (list)))


(define/spec (value-equiv? val1 val2 ids1 ids2)
  (-> Value?
      Value?
      (listof (list Id? Id?))
      (listof (list Id? Id?))
      boolean?)
  ;; match* lets us match on multiple expressions
  ;; at once -- here we're matching on val1 and val2,
  ;; and so each clause must have a sequence with
  ;; two match patterns
  (match* (val1 val2)
    [((CLOSURE ρ1 x1 body1)
      (CLOSURE ρ2 x2 body2))
     (define fresh (gensym))
     (and (ρ-equiv? ρ1 ρ2 ids1 ids2)
          (expr-equiv? body1 body2
                       (cons (list x1 fresh) ids1)
                       (cons (list x2 fresh) ids2)))]))



(define/spec (id-equiv? id1 id2 ids1 ids2)
  (-> Id?
      Id?
      (listof (list Id? Id?))
      (listof (list Id? Id?))
      boolean?)
  (define entry1 (assoc id1 ids1))
  (define entry2 (assoc id2 ids2))
  (define id1^ (or (and entry1 (second entry1)) id1))
  (define id2^ (or (and entry2 (second entry2)) id2))
  (equal? id1^ id2^))


(define/spec (ρ-equiv? ρ1 ρ2 ids1 ids2)
  (-> ValueEnv?
      ValueEnv?
      (listof (list Id? Id?))
      (listof (list Id? Id?))
      boolean?)
  (and (= (length ρ1) (length ρ2))
       (andmap (λ (entry1 entry2)
                 (match* (entry1 entry2)
                   [(`(,id1 ,val1)
                     `(,id2 ,val2))
                    (and (id-equiv? id1 id2 ids1 ids2)
                         (value-equiv? val1 val2 ids1 ids2))]))
               ρ1 ρ2)))


(define/spec (expr-equiv? expr1 expr2 ids1 ids2)
  (-> Expr?
      Expr?
      (listof (list Id? Id?))
      (listof (list Id? Id?))
      boolean?)
  (match* (expr1 expr2)
    [(id1 id2)
     #:when (and (symbol? id1) (symbol? id2))
     (id-equiv? id1 id2 ids1 ids2)]
    [(`(λ (,x1) ,body1)
      `(λ (,x2) ,body2))
     (define fresh (gensym))
     (expr-equiv? body1 body2
                  (cons (list x1 fresh) ids1)
                  (cons (list x2 fresh) ids2))]
    [(`(,rator1 ,rand1)
      `(,rator2 ,rand2))
     (and (expr-equiv? rator1 rator2 ids1 ids2)
          (expr-equiv? rand1 rand2 ids1 ids2))]
    [(_ _) #f]))


;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Exercise 2
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Define two expressions e1 and e2 which evaluate to
;; church numerals, for which
;;
;; (equal? (valof-program '() (with-church-numerals e1))
;;         (valof-program '() (with-church-numerals e2)))
;;
;; produces #f, but for which
;;
;; (α/value=? (valof-program '() (with-church-numerals e1))
;;            (valof-program '() (with-church-numerals e2)))
;;
;; produces #t.
;;
;; e.g., '(λ (x) x) and '(λ (y) y) are not equal?, but they
;; are α/value=? (note that neither of these are church
;; numerals, however, and your expressions should be
;; church numerals).
;; 
;; Use the check-equal? tests commented out below to verify
;; your e1 and e2 meet the above criteria.
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(define e1 '(λ (f) (λ (x) (f x))))
(define e2 '(λ (g) (λ (x) (g x))))
(check-equal?
 (equal? (valof-program '() (with-church-numerals e1))
         (valof-program '() (with-church-numerals e2)))
 #f)
(check-equal?
 (α/value=? (car (valof-program '() (with-church-numerals e1)))
            (car (valof-program '() (with-church-numerals e2))))
 #t)

;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Exercise 3
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;; Define two expressions e3 and e4 that evaluate to church
;; numerals, for which
;;
;; (equal? (valof-program '() (with-church-numerals e3))
;;         (valof-program '() (with-church-numerals e4)))
;;
;; produces #f and for which
;;
;; (α/value=? (valof-program '() (with-church-numerals e3))
;;            (valof-program '() (with-church-numerals e4)))
;;
;; produces #f, but which semantically are the same (i.e.
;; they represent the same church-numeral, i.e. they are
;; functions which would behave the same when given the
;; same input).
;;
;; e.g., '(λ (x) x) and '(λ (y) ((λ (z) z) y)) are not equal?
;; or α/value=?, but both are different ways of writing
;; a function that simply returns its input.
;;
;; Use the check-equal? tests commented out below to verify
;; your e3 and e4 meet the above criteria, and describe
;; below in a comment why the two expressions are
;; semantically the same.
;;
;; Hint: Can you use church-plus to define two expressions
;; which would evaluate to the same church numeral (e.g. both
;; would equal 4), but whose actual representation as a value
;; differs in some way that makes them not equal? or α/value=?
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
(define e3 `((church-plus ,(to-church 1)) ,(to-church 2)))
(define e4 (to-church 3))
(check-equal?
 (equal? (valof-program '() (with-church-numerals e3))
         (valof-program '() (with-church-numerals e4)))
 #f)
#|
((church-plus (church-add1 church-zero))
 (church-add1 (church-add1 church-zero)))|#
(check-equal?
 (α/value=? (car (valof-program '() (with-church-numerals e3)))
            (car (valof-program '() (with-church-numerals e4))))
 #f)
;(church-add1 (church-add1 (church-add1 church-zero)))