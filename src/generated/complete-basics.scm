;; First include the module loading code
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define objects
;; Create objects
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))

;; Create morphisms
(define f (make-morphism "f" a b))
(define g (make-morphism "g" b c))

;; Create identity morphisms
(define id-a (category-identity a))
(define id-b (category-identity b))
(define id-c (category-identity c))

;; Create composition
(define g-after-f (category-compose f g))

;; Extra objects for isomorphism example
(define x (make-object "X"))
(define y (make-object "Y"))
(define f-xy (make-morphism "f" x y))
(define g-yx (make-morphism "g" y x))
(define f-after-g (category-compose g-yx f-xy))
(define g-after-f (category-compose f-xy g-yx))

;; Display results
(display "=== CATEGORY THEORY BASICS ===\n\n")

(display "OBJECTS:\n")
(display a) (newline)
(display b) (newline)
(display c) (newline)
(display x) (newline)
(display y) (newline)

(display "\nMORPHISMS:\n")
(display f) (newline)
(display g) (newline)
(display f-xy) (newline)
(display g-yx) (newline)

(display "\nIDENTITY MORPHISMS:\n")
(display id-a) (newline)
(display id-b) (newline)
(display id-c) (newline)

(display "\nCOMPOSITIONS:\n")
(display "g ∘ f: ") (display g-after-f) (newline)
(display "f ∘ g: ") (display f-after-g) (newline)
(display "f-xy ∘ g-yx: ") (display f-after-g) (newline)
(display "g-yx ∘ f-xy: ") (display g-after-f) (newline)

;; Create a category
(define simple-cat 
  (make-category "SimpleCategory" 
                 (list a b c) 
                 (list f g id-a id-b id-c g-after-f)))

(display "\nCATEGORY:\n")
(display simple-cat) (newline)
