;; First load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a simple category
(define a (make-object "A"))
(define b (make-object "B"))
(define f (make-morphism "f" a b))

;; Yoneda embedding maps object A to the functor Hom(-, A)
;; We'll represent this mapping symbolically
(display "Yoneda embedding:\n")
(display "y(A) = Hom(-, A)\n")
(display "y(B) = Hom(-, B)\n")

;; For a morphism f: A → B, Yoneda maps it to a natural transformation
;; y(f): Hom(-, A) ⇒ Hom(-, B)
(display "\nYoneda embedding of morphism f: A → B:\n")
(display "y(f): Hom(-, A) ⇒ Hom(-, B)\n")

;; For any object X, y(f)_X: Hom(X, A) → Hom(X, B) is given by 
;; composition with f
(display "\nFor any object X, y(f)_X maps each morphism g: X → A to f ∘ g: X → B\n")

;; Symbolic example
(display "\nExample: For a morphism h: C → A\n")
(display "y(f)_C(h) = f ∘ h: C → B\n")

;; This demonstrates that y is a functor:
;; - It maps objects to objects (in the functor category)
;; - It maps morphisms to morphisms (natural transformations)
;; - It preserves composition and identities
