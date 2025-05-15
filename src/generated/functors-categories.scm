;; First include the module loading code
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Category C
(define c-obj1 (make-object "C1"))
(define c-obj2 (make-object "C2"))
(define c-mor (make-morphism "f" c-obj1 c-obj2))
(define c-id1 (identity c-obj1))
(define c-id2 (identity c-obj2))
(define cat-c (make-category "C" (list c-obj1 c-obj2) (list c-mor c-id1 c-id2)))

;; Category D
(define d-obj1 (make-object "D1"))
(define d-obj2 (make-object "D2"))
(define d-mor (make-morphism "g" d-obj1 d-obj2))
(define d-id1 (identity d-obj1))
(define d-id2 (identity d-obj2))
(define cat-d (make-category "D" (list d-obj1 d-obj2) (list d-mor d-id1 d-id2)))

;; Display our categories
(display "Category C:\n")
(display cat-c) (newline)
(display "\nCategory D:\n")
(display cat-d) (newline)
