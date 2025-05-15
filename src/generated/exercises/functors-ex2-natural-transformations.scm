;; First include the module loading code
(add-to-load-path "../src/guile")
(use-modules (category))

;; Create a simple category
(define c-a (make-object "A"))
(define c-b (make-object "B"))
(define c-f (make-morphism "f" c-a c-b))
(define c-cat (make-category "C" (list c-a c-b) (list c-f)))

;; Create a target category
(define d-x (make-object "X"))
(define d-y (make-object "Y"))
(define d-z (make-object "Z"))
(define d-p (make-morphism "p" d-x d-y))
(define d-q (make-morphism "q" d-y d-z))
(define d-cat (make-category "D" (list d-x d-y d-z) (list d-p d-q)))

;; Define first functor F: C → D
(define (functor-f-obj obj)
  (cond ((equal? obj c-a) d-x)
        ((equal? obj c-b) d-z)
        (else (error "Object not in source category"))))

(define (functor-f-mor mor)
  (let ((name (cadr mor)))
    (cond ((equal? name "f") (compose d-p d-q))  ;; F maps f to q ∘ p
          (else (error "Morphism not in source category")))))

;; Define second functor G: C → D
(define (functor-g-obj obj)
  (cond ((equal? obj c-a) d-y)  ;; Different from F
        ((equal? obj c-b) d-z)  ;; Same as F
        (else (error "Object not in source category"))))

(define (functor-g-mor mor)
  (let ((name (cadr mor)))
    (cond ((equal? name "f") d-q)  ;; G maps f to q only
          (else (error "Morphism not in source category")))))

;; Natural transformation α: F ⇒ G
;; For each object X in C, we need a morphism α_X: F(X) → G(X)
(define (alpha obj)
  (cond ((equal? obj c-a) d-p)           ;; α_A: F(A) → G(A), or X → Y
        ((equal? obj c-b) (identity d-z)) ;; α_B: F(B) → G(B), or Z → Z (identity)
        (else (error "Object not in source category"))))

;; Natural transformation β: G ⇒ F
;; For each object X in C, we need a morphism β_X: G(X) → F(X)
;; Note: This won't be a complete natural transformation since there's no morphism
;; from Y to X in our example category D. This is intentional to show the constraints.
(define (beta obj)
  (cond ((equal? obj c-a) (make-morphism "impossible" d-y d-x))  ;; β_A: G(A) → F(A), or Y → X (would need to exist)
        ((equal? obj c-b) (identity d-z))                       ;; β_B: G(B) → F(B), or Z → Z (identity)
        (else (error "Object not in source category"))))

;; Demonstrate the natural transformations
(display "NATURAL TRANSFORMATIONS DEMONSTRATION\n\n")

(display "Functor F mappings:\n")
(display "F(A) = ") (display (functor-f-obj c-a)) (newline)
(display "F(B) = ") (display (functor-f-obj c-b)) (newline)
(display "F(f) = ") (display (functor-f-mor c-f)) (newline)

(display "\nFunctor G mappings:\n")
(display "G(A) = ") (display (functor-g-obj c-a)) (newline)
(display "G(B) = ") (display (functor-g-obj c-b)) (newline)
(display "G(f) = ") (display (functor-g-mor c-f)) (newline)

(display "\nNatural transformation α: F ⇒ G components:\n")
(display "α_A: F(A) → G(A) = ") (display (alpha c-a)) (newline)
(display "α_B: F(B) → G(B) = ") (display (alpha c-b)) (newline)

;; Check naturality condition: α_B ∘ F(f) = G(f) ∘ α_A
(define left-side (compose (functor-f-mor c-f) (alpha c-b)))
(define right-side (compose (alpha c-a) (functor-g-mor c-f)))

(display "\nChecking naturality condition for α:\n")
(display "α_B ∘ F(f) = ") (display left-side) (newline)
(display "G(f) ∘ α_A = ") (display right-side) (newline)
