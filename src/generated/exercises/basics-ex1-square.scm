;; First include the module loading code
(add-to-load-path "../src/guile")
(use-modules (category))

;; Define square category objects
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))
(define d (make-object "D"))

;; Create morphisms for the square
(define f-ab (make-morphism "f_AB" a b))
(define f-bc (make-morphism "f_BC" b c))
(define f-cd (make-morphism "f_CD" c d))
(define f-da (make-morphism "f_DA" d a))

;; Create diagonal morphisms
(define f-ac (make-morphism "f_AC" a c))
(define f-bd (make-morphism "f_BD" b d))

;; Create compositions
(define f-ab-bc (category-compose f-ab f-bc))  ;; Should be equivalent to f-ac
(define f-bc-cd (category-compose f-bc f-cd))  ;; Should be equivalent to f-bd
(define f-cd-da (category-compose f-cd f-da))  ;; Should be equivalent to a loop
(define f-da-ab (category-compose f-da f-ab))  ;; Should be equivalent to a loop

;; Create a category
(define square-cat 
  (make-category "SquareCategory" 
                 (list a b c d) 
                 (list f-ab f-bc f-cd f-da f-ac f-bd)))

;; Display the category and compositions
(display "SQUARE CATEGORY:\n")
(display square-cat) (newline)

(display "\nCOMPOSITIONS:\n")
(display "f_BC ∘ f_AB: ") (display f-ab-bc) (newline)
(display "f_CD ∘ f_BC: ") (display f-bc-cd) (newline)
(display "f_DA ∘ f_CD: ") (display f-cd-da) (newline)
(display "f_AB ∘ f_DA: ") (display f-da-ab) (newline)
