;; First load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a very simple category with a few objects and morphisms
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))

(define f (make-morphism "f" a b))
(define g (make-morphism "g" b c))
(define h (make-morphism "h" a c))

;; A representable functor Hom(A, -) maps:
;; - Each object X to the set of morphisms from A to X
;; - Each morphism m: X → Y to the function that composes m with morphisms A → X

;; Represent Hom(A, X) as a list of morphisms
(define (hom a x category-morphisms)
  (filter (lambda (m)
            (and (equal? (caddr m) a)  ; domain is A
                 (equal? (cadddr m) x))) ; codomain is X
          category-morphisms))

;; All morphisms in our simple category
(define all-morphisms (list f g h))

;; Display representable functor values
(display "Representable functor Hom(A, -):\n")
(display "Hom(A, A): ") (display (hom a a all-morphisms)) (newline)
(display "Hom(A, B): ") (display (hom a b all-morphisms)) (newline)
(display "Hom(A, C): ") (display (hom a c all-morphisms)) (newline)

;; Function that maps morphisms under the representable functor
(define (functor-map-morphism m hom-a-x)
  (map (lambda (morphism-from-a)
         (compose morphism-from-a m))
       hom-a-x))

;; Example: Map morphism g: B → C on Hom(A, B)
(display "\nApplying g: B → C to Hom(A, B):\n")
(let ((hom-a-b (hom a b all-morphisms)))
  (display "Result: ")
  (display (functor-map-morphism g hom-a-b))
  (newline))
