  ;; Simple type inference example
  (define (infer-type expr env)
    (match expr
      [(? number?) 'number]
      [(? boolean?) 'boolean]
      [(? symbol?) (lookup-type expr env)]
      [`(lambda (,x) ,body)
       (let ([type-var (make-type-var)]
             [new-env (extend-env env x type-var)])
         `(-> ,type-var ,(infer-type body new-env)))]
      [`(,fun ,arg)
       (let ([fun-type (infer-type fun env)]
             [arg-type (infer-type arg env)])
         (match fun-type
           [`(-> ,param-type ,result-type)
            (if (can-unify? param-type arg-type)
                result-type
                (error "Type mismatch"))]
           [_ (error "Non-function in application position")]))]))

  ;; Simplified dependent type checking
  (define-record-type <dependent-type>
    (make-dep-type kind expr)
    dep-type?
    (kind dep-type-kind)
    (expr dep-type-expr))
  
  ;; Vec type: Vector of specific length
  (define (vec-type elem-type length)
    (make-dep-type 'vec (list elem-type length)))
  
  ;; Type checking vec-cons operation
  (define (check-vec-cons elem vec)
    (let ([elem-type (infer-type elem)]
          [vec-type (infer-type vec)])
      (match vec-type
        [(? dep-type? t)
         (if (eq? (dep-type-kind t) 'vec)
             (let ([stored-type (car (dep-type-expr t))]
                   [length (cadr (dep-type-expr t))])
               (if (can-unify? elem-type stored-type)
                   (vec-type stored-type (+ length 1))
                   (error "Element type mismatch")))
             (error "Not a vector type"))]
        [_ (error "Not a vector")])))

  ;; Generalized Algebraic Data Types example
  ;; Represents expressions with type safety
  (define-type Expr
    (U (Struct 'Lit Integer)                  ; Lit : Integer -> Expr Integer
       (Struct 'Add (Expr Integer) (Expr Integer))  ; Add : Expr Integer -> Expr Integer -> Expr Integer
       (Struct 'Eq (Expr Integer) (Expr Integer))   ; Eq : Expr Integer -> Expr Integer -> Expr Boolean
       (Struct 'If (Expr Boolean) (All (a) (Expr a)) (All (a) (Expr a))))) ; If : Expr Boolean -> Expr a -> Expr a -> Expr a
  
  ;; Type-safe evaluation function
  (: eval : (All (a) (Expr a) -> a))
  (define (eval expr)
    (match expr
      [(struct Lit (n)) n]
      [(struct Add (e1 e2)) (+ (eval e1) (eval e2))]
      [(struct Eq (e1 e2)) (= (eval e1) (eval e2))]
      [(struct If (cond then else)) (if (eval cond) (eval then) (eval else))]))

  ;; Linear types simulation
  (define-record-type <linear>
    (make-linear value consumed)
    linear?
    (value linear-value)
    (consumed linear-consumed)
    (consumed-set! linear-consumed-set!))
  
  ;; Linear function example (consumes its argument)
  (define (linear-consume resource)
    (if (linear-consumed resource)
        (error "Resource already consumed")
        (begin
          (linear-consumed-set! resource #t)
          (linear-value resource))))
  
  ;; Example usage with file handles
  (define (with-file filename proc)
    (let ([file (make-linear (open-input-file filename) #f)])
      (dynamic-wind
        (lambda () #f)
        (lambda () (proc file))
        (lambda () 
          (when (not (linear-consumed file))
            (close-input-port (linear-value file))
            (linear-consumed-set! file #t))))))

  ;; List functor
  (define (list-map f lst)
    (map f lst))
  
  ;; Maybe functor
  (define-record-type <maybe>
    (make-maybe value)
    maybe?
    (value maybe-value))
  
  (define nothing 'nothing)
  
  (define (just x)
    (make-maybe x))
  
  (define (maybe-map f maybe-val)
    (if (eq? maybe-val nothing)
        nothing
        (just (f (maybe-value maybe-val)))))
  
  ;; Demonstrating functor laws
  (define (test-functor-laws)
    (let ([test-list '(1 2 3)]
          [test-maybe (just 42)]
          [id (lambda (x) x)]
          [f (lambda (x) (+ x 1))]
          [g (lambda (x) (* x 2))])
      ;; Identity law: map id = id
      (and (equal? (list-map id test-list) test-list)
           (or (and (eq? test-maybe nothing) 
                    (eq? (maybe-map id test-maybe) nothing))
               (equal? (maybe-value (maybe-map id test-maybe)) 
                       (maybe-value test-maybe)))
           ;; Composition law: map (g . f) = (map g) . (map f)
           (equal? (list-map (lambda (x) (g (f x))) test-list)
                   (list-map g (list-map f test-list)))
           (or (and (eq? test-maybe nothing)
                    (eq? (maybe-map (lambda (x) (g (f x))) test-maybe)
                         (maybe-map g (maybe-map f test-maybe))))
               (equal? (maybe-value (maybe-map (lambda (x) (g (f x))) test-maybe))
                       (maybe-value (maybe-map g (maybe-map f test-maybe))))))))

  ;; Natural transformation from List to Maybe (safe-head)
  (define (safe-head lst)
    (if (null? lst)
        nothing
        (just (car lst))))
  
  ;; Natural transformation from Maybe to List
  (define (maybe-to-list maybe-val)
    (if (eq? maybe-val nothing)
        '()
        (list (maybe-value maybe-val))))
  
  ;; Testing naturality
  (define (test-naturality)
    (let ([test-list '(1 2 3)]
          [f (lambda (x) (* x 2))])
      ;; Naturality condition for safe-head:
      ;; safe-head . map f = fmap f . safe-head
      (equal? (safe-head (list-map f test-list))
              (maybe-map f (safe-head test-list)))))

  ;; Product type
  (define-record-type <pair>
    (make-pair first second)
    pair?
    (first pair-first)
    (second pair-second))
  
  ;; Product operations
  (define (pair-map f g p)
    (make-pair (f (pair-first p))
               (g (pair-second p))))
  
  ;; Coproduct type (Either)
  (define-record-type <either>
    (make-either tag value)
    either?
    (tag either-tag)
    (value either-value))
  
  (define (left x)
    (make-either 'left x))
  
  (define (right x)
    (make-either 'right x))
  
  ;; Coproduct operations
  (define (either-map f g e)
    (case (either-tag e)
      [(left) (left (f (either-value e)))]
      [(right) (right (g (either-value e)))]))

  ;; Monad operations for Maybe
  (define (maybe-return x)
    (just x))
  
  (define (maybe-bind maybe-val f)
    (if (eq? maybe-val nothing)
        nothing
        (f (maybe-value maybe-val))))
  
  ;; Kleisli composition for Maybe
  (define (kleisli-compose f g)
    (lambda (x)
      (maybe-bind (f x) g)))
  
  ;; Testing monad laws
  (define (test-monad-laws)
    (let ([x 42]
          [f (lambda (n) (just (+ n 1)))]
          [g (lambda (n) (just (* n 2)))])
      ;; Left identity: return a >>= f = f a
      (equal? (maybe-bind (maybe-return x) f)
              (f x))
      ;; Right identity: m >>= return = m
      (equal? (maybe-bind (just x) maybe-return)
              (just x))
      ;; Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
      (equal? (maybe-bind (maybe-bind (just x) f) g)
              (maybe-bind (just x) (lambda (y) (maybe-bind (f y) g))))))

  ;; Logic connectives as types
  
  ;; Conjunction (AND) as product type
  (define-record-type <and-proof>
    (make-and-proof left right)
    and-proof?
    (left and-proof-left)
    (right and-proof-right))
  
  ;; Disjunction (OR) as sum type
  (define-record-type <or-proof>
    (make-or-proof tag value)
    or-proof?
    (tag or-proof-tag)
    (value or-proof-value))
  
  (define (or-left x)
    (make-or-proof 'left x))
  
  (define (or-right x)
    (make-or-proof 'right x))
  
  ;; Implication (→) as function type
  (define (implication-proof p q)
    (lambda (proof-of-p) (q proof-of-p)))
  
  ;; Universal quantification (∀) as polymorphic function
  (define (forall-proof p)
    (lambda (type) (p type)))
  
  ;; Existential quantification (∃) as dependent pair
  (define-record-type <exists-proof>
    (make-exists-proof witness proof)
    exists-proof?
    (witness exists-proof-witness)
    (proof exists-proof-proof))

  ;; Proving A → (B → A)
  ;; In logic: A implies (B implies A)
  ;; In types: A → (B → A)
  (define (proof-k a)
    (lambda (b) a))
  
  ;; Proving (A → (B → C)) → ((A → B) → (A → C))
  ;; This is the S combinator in SKI calculus
  (define (proof-s f)
    (lambda (g)
      (lambda (a)
        ((f a) (g a)))))
  
  ;; Proving ((A → B) → A) → A
  ;; This is Peirce's law in classical logic
  ;; Not provable in constructive logic!
  ;; In types: We need continuations or other classical features
  (define (proof-peirce-with-callcc p)
    (call/cc
     (lambda (k)
       (p (lambda (a) (k a))))))

  ;; De Morgan's laws as type isomorphisms
  
  ;; not(A and B) ≅ not(A) or not(B)
  (define (demorgan1-to f)
    (match f
      [(lambda (p) (error "Contradiction"))
       (if (and-proof? p)
           (let ([a (and-proof-left p)]
                 [b (and-proof-right p)])
             ;; Either not(A) or not(B)
             (try
              (or-left (lambda () (f (make-and-proof a b))))
              (or-right (lambda () (f (make-and-proof a b))))))
           (error "Not an and-proof"))]))
  
  (define (demorgan1-from or-not)
    (lambda (p)
      (match or-not
        [(? or-proof? o)
         (case (or-proof-tag o)
           [(left) ((or-proof-value o) (and-proof-left p))]
           [(right) ((or-proof-value o) (and-proof-right p))])]
        [_ (error "Not an or-proof")])))

  ;; Recursively defined types
  (define-record-type <mu>
    (make-mu unroll)
    mu?
    (unroll mu-unroll))
  
  ;; List type as fixed point
  (define-record-type <list-f>
    (make-list-f tag value)
    list-f?
    (tag list-f-tag)
    (value list-f-value))
  
  (define (nil)
    (make-mu (make-list-f 'nil '())))
  
  (define (cons head tail)
    (make-mu (make-list-f 'cons (cons head tail))))
  
  ;; Extract values from our recursive list
  (define (list-case l nil-case cons-case)
    (let ([unrolled (mu-unroll l)])
      (case (list-f-tag unrolled)
        [(nil) (nil-case)]
        [(cons) (let ([pair (list-f-value unrolled)])
                  (cons-case (car pair) (cdr pair)))])))

  ;; Type-level fixed point operator
  (define (fix-type f)
    (lambda (x) ((f (fix-type f)) x)))
  
  ;; List type as fixed point of a functor
  (define list-type
    (fix-type
     (lambda (rec)
       (lambda (elem-type)
         `(either unit (pair ,elem-type ,rec))))))
  
  ;; Tree type as fixed point of a functor
  (define tree-type
    (fix-type
     (lambda (rec)
       (lambda (elem-type)
         `(either ,elem-type (pair ,rec ,rec))))))

  ;; A simplified typeful quine
  ;; This is a rough approximation as true typeful quines
  ;; require more sophisticated type systems
  (define (typed-quine)
    (let* ([code '(lambda (x) 
                    (let ([code x]) 
                      (list code code)))]
           [program (eval code)])
      (program code)))
  
  ;; More explicit typing (pseudo-code)
  #|
  type Quine a = (Quine a -> a) -> a
  
  quine : Quine (Quine a)
  quine = λself. self self
  |#

  ;; Pseudo-implementation of Gödel numbering for types
  (define (type->number type)
    (match type
      ['unit 0]
      [(list 'pair t1 t2) (+ (* 3 (type->number t1))
                             (* 5 (type->number t2)))]
      [(list 'arrow t1 t2) (+ (* 7 (type->number t1))
                              (* 11 (type->number t2)))]
      [(list 'forall var body) (+ (* 13 (symbol->number var))
                                  (* 17 (type->number body)))]))
  
  ;; Pseudo-implementation of self-reference at type level
  (define (make-self-referential type-function)
    (let* ([type-code (type->number type-function)]
           [type-with-hole (insert-type-code-hole type-function)]
           [self-ref-type (insert-code type-with-hole type-code)])
      self-ref-type))
