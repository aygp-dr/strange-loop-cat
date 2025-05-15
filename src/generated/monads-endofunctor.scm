;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a simple category
(define a (make-object "A"))
(define b (make-object "B"))
(define f (make-morphism "f" a b))

;; An endofunctor T maps:
;; - Objects to objects in the same category
;; - Morphisms to morphisms, preserving structure

;; For our simple example, we'll define an endofunctor T as:
;; T(A) = A
;; T(B) = B
;; T(f: A → B) = f: A → B

(define (T-obj obj)
  ;; This is the identity mapping for simplicity
  obj)

(define (T-mor mor)
  ;; This is the identity mapping for simplicity
  mor)

;; Display the endofunctor's action
(display "Endofunctor T maps:\n")
(display "T(A) = ") (display (T-obj a)) (newline)
(display "T(B) = ") (display (T-obj b)) (newline)
(display "T(f: A → B) = ") (display (T-mor f)) (newline)

;; A more interesting endofunctor might "wrap" objects and morphisms
;; For instance, the "option" or "maybe" endofunctor in programming
