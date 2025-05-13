;; Create objects
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))

;; First include the module loading code
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Then include object definitions
;; Create objects
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))

;; Create morphisms
(define f (make-morphism "f" a b))
(define g (make-morphism "g" b c))

;; Display our objects and morphisms
(display "Objects:\n")
(display a) (newline)
(display b) (newline)
(display c) (newline)

(display "\nMorphisms:\n")
(display f) (newline)
(display g) (newline)
