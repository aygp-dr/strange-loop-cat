;; First include the module loading code
(add-to-load-path "../src/guile")
(use-modules (category))

;; Create a simple source category
(define s-a (make-object "A"))
(define s-b (make-object "B"))
(define s-c (make-object "C"))
(define s-f (make-morphism "f" s-a s-b))
(define s-g (make-morphism "g" s-b s-c))
(define s-cat (make-category "Source" 
                           (list s-a s-b s-c) 
                           (list s-f s-g)))

;; Create a simple target category
(define t-x (make-object "X"))
(define t-y (make-object "Y"))
(define t-z (make-object "Z"))
(define t-p (make-morphism "p" t-x t-y))
(define t-q (make-morphism "q" t-y t-z))
(define t-cat (make-category "Target" 
                           (list t-x t-y t-z) 
                           (list t-p t-q)))

;; Contravariant functor maps objects
;; The key is that it reverses the direction of morphisms
(define (contra-functor-obj obj)
  (cond ((equal? obj s-a) t-z)       ;; A → Z (flipped)
        ((equal? obj s-b) t-y)       ;; B → Y (flipped)
        ((equal? obj s-c) t-x)       ;; C → X (flipped)
        (else (error "Object not in source category"))))

;; Contravariant functor maps morphisms - REVERSING DIRECTIONS
(define (contra-functor-mor mor)
  (let ((name (cadr mor))
        (domain (caddr mor))
        (codomain (cadddr mor)))
    (cond ((equal? name "f") t-q)    ;; f: A → B maps to q: Y → Z (but reversed: Z ← Y)
          ((equal? name "g") t-p)    ;; g: B → C maps to p: X → Y (but reversed: Y ← X)
          (else (error "Morphism not in source category")))))

;; Test contravariant functor
(display "CONTRAVARIANT FUNCTOR DEMONSTRATION\n\n")

(display "Source category objects and morphisms:\n")
(display "s-a: ") (display s-a) (newline)
(display "s-b: ") (display s-b) (newline)
(display "s-c: ") (display s-c) (newline)
(display "s-f (A → B): ") (display s-f) (newline)
(display "s-g (B → C): ") (display s-g) (newline)

(display "\nTarget category objects and morphisms:\n")
(display "t-x: ") (display t-x) (newline)
(display "t-y: ") (display t-y) (newline)
(display "t-z: ") (display t-z) (newline)
(display "t-p (X → Y): ") (display t-p) (newline)
(display "t-q (Y → Z): ") (display t-q) (newline)

(display "\nContravariant functor mappings:\n")
(display "F(A) = ") (display (contra-functor-obj s-a)) (newline)
(display "F(B) = ") (display (contra-functor-obj s-b)) (newline)
(display "F(C) = ") (display (contra-functor-obj s-c)) (newline)
(display "F(f: A → B) = ") (display (contra-functor-mor s-f)) 
(display " (reversed: Z ← Y)") (newline)
(display "F(g: B → C) = ") (display (contra-functor-mor s-g))
(display " (reversed: Y ← X)") (newline)

;; Check compositional property: F(g ∘ f) = F(f) ∘ F(g) - order reversed!
(define s-g-f (compose s-f s-g))
(define t-p-q (compose t-q t-p))  ;; Note the reverse order!

(display "\nTesting compositional property:\n")
(display "g ∘ f in source: ") (display s-g-f) (newline)
(display "F(g ∘ f) should equal p ∘ q (reversed order!): ")
(display (contra-functor-mor s-g-f)) (newline)
(display "p ∘ q in target: ") (display t-p-q) (newline)
