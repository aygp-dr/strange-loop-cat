;; First include categories definition
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

;; Define a functor as a mapping of objects and morphisms
(define (functor-obj obj)
  (cond ((equal? obj c-obj1) d-obj1)
        ((equal? obj c-obj2) d-obj2)
        (else (error "Object not in source category"))))

(define (functor-mor mor)
  (let ((name (cadr mor))
        (domain (caddr mor))
        (codomain (cadddr mor)))
    (cond ((equal? name "f") d-mor)
          ((equal? name "id_C1") d-id1)
          ((equal? name "id_C2") d-id2)
          (else (error "Morphism not in source category")))))

;; Apply functor to objects
(display "Applying functor to objects:\n")
(display "F(C1) = ") (display (functor-obj c-obj1)) (newline)
(display "F(C2) = ") (display (functor-obj c-obj2)) (newline)

;; Apply functor to morphisms
(display "\nApplying functor to morphisms:\n")
(display "F(f: C1 → C2) = ") (display (functor-mor c-mor)) (newline)
(display "F(id_C1) = ") (display (functor-mor c-id1)) (newline)
