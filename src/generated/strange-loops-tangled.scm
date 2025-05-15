;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a simple category
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))

;; Create morphisms for direct self-reference
(define f-aa (make-morphism "f_AA" a a))  ;; Endomorphism

;; Create morphisms for a loop
(define f-ab (make-morphism "f_AB" a b))
(define f-bc (make-morphism "f_BC" b c))
(define f-ca (make-morphism "f_CA" c a))

;; Display the different types of strange loops
(display "Types of Strange Loops:\n")

(display "\n1. Direct Self-Reference:\n")
(display "   Endomorphism f_AA: A → A\n")
(display "   This is a morphism from an object to itself.\n")

(display "\n2. Circular Loop:\n")
(display "   f_AB: A → B, f_BC: B → C, f_CA: C → A\n")
(display "   Following these morphisms forms a loop: A → B → C → A\n")

;; Simulating a level-crossing strange loop
(display "\n3. Tangled Hierarchy (simulated):\n")
(display "   Consider a category of 'levels' where:\n")
(display "   - Level 1 contains basic elements\n")
(display "   - Level 2 contains operations on Level 1 elements\n")
(display "   - Level 3 contains meta-operations on Level 2 operations\n")
(display "   A tangled hierarchy occurs when an element at Level 3\n")
(display "   is equivalent to an element at Level 1, creating a loop\n")
(display "   across levels that should be hierarchically separate.\n")

;; Simulation of a tangled hierarchy using category levels
(display "Tangled Hierarchies - Categorical Example:\n")

(display "\nLevel structure (simplified):\n")
(display "Level 1: Category C1 of basic objects\n")
(display "Level 2: Category C2 of functors between C1 objects\n")
(display "Level 3: Category C3 of natural transformations between functors\n")

(display "\nIn a normal hierarchy, each level operates on the level below it.\n")

(display "\nIn a tangled hierarchy, there's a way to interpret:\n")
(display "- Elements of Level 3 as elements of Level 1\n")
(display "- This creates a loop where following the hierarchy upward\n")
(display "  eventually brings you back to the bottom\n")

(display "\nCategorical example:\n")
(display "- Level 1: Category C\n")
(display "- Level 2: Functor category [C,C] of endofunctors on C\n")
(display "- Level 3: Natural transformations between endofunctors\n")
(display "Tangling: A representation of natural transformations as objects in C\n")
