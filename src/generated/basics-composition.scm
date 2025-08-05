;; First include all the necessary definitions
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")
;; Create objects
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))

;; Create morphisms needed for composition
(define f (make-morphism "f" a b))
(define g (make-morphism "g" b c))

;; Compose morphisms g and f
(define g-after-f (category-compose f g))

(display "Composition g ∘ f: ")
(display g-after-f)
(newline)
