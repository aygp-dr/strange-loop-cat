;; First load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a simple category
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))
(define f (make-morphism "f" a b))
(define g (make-morphism "g" b c))

;; Suppose we have a functor F: C → Set
;; For simplicity, we'll define F symbolically:
(display "Functor F maps:\n")
(display "F(A) = {a1, a2}\n")
(display "F(B) = {b1, b2, b3}\n")
(display "F(C) = {c1, c2, c4}\n")

;; The Yoneda Lemma states that natural transformations 
;; from Hom(A, -) to F correspond bijectively to elements of F(A)
(display "\nYoneda Lemma states: Nat(Hom(A, -), F) ≅ F(A)\n")

;; How does this bijection work?
(display "\nFor an element a1 ∈ F(A), the corresponding natural transformation α is:\n")
(display "For any object X and any morphism h: A → X,\n")
(display "α_X(h) = F(h)(a1)\n")

;; Example: For the morphism f: A → B
(display "\nExample: For morphism f: A → B\n")
(display "α_B(f) = F(f)(a1) ∈ F(B)\n")

;; Conversely, given a natural transformation β: Hom(A, -) ⇒ F,
;; we get an element of F(A) by evaluating at the identity of A
(display "\nConversely, for a natural transformation β: Hom(A, -) ⇒ F\n")
(display "The corresponding element of F(A) is: β_A(id_A)\n")

;; This bijection is profound: it means that knowing all the ways objects
;; relate to A (via Hom(A, -)) is equivalent to knowing the "value" of
;; any functor F at A
(display "\nThe Yoneda Lemma thus tells us that an object A is completely\n")
(display "determined by how it relates to all other objects (via morphisms).\n")
