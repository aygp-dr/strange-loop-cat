;; Create identity morphisms
(define id-a (category-identity a))
(define id-b (category-identity b))
(define id-c (category-identity c))

(display "Identity morphisms:\n")
(display id-a) (newline)
(display id-b) (newline)
(display id-c) (newline)

;; Define some sets (as lists for simplicity)
(define set-a '(1 2 3))
(define set-b '("a" "b"))
(define set-c '(#t #f))

;; Define functions (these are simplified for demonstration)
(define (func-f x)
  (if (= x 1) "a" "b"))

(define (func-g y)
  (if (string=? y "a") #t #f))

;; Manually apply functions to elements
(display "Applying f to elements of A:\n")
(for-each (lambda (x)
            (format #t "f(~a) = ~a\n" x (func-f x)))
          set-a)

(display "\nApplying g to elements of B:\n")
(for-each (lambda (y)
            (format #t "g(~a) = ~a\n" y (func-g y)))
          set-b)

;; Composing functions
(define (g-compose-f x)
  (func-g (func-f x)))

(display "\nApplying g ∘ f to elements of A:\n")
(for-each (lambda (x)
            (format #t "(g ∘ f)(~a) = ~a\n" x (g-compose-f x)))
          set-a)

;; Create objects
(define x (make-object "X"))
(define y (make-object "Y"))

;; Create isomorphic morphisms
(define f-xy (make-morphism "f" x y))
(define g-yx (make-morphism "g" y x))

;; Check compositions
(define f-after-g (category-compose g-yx f-xy))
(define g-after-f (category-compose f-xy g-yx))

(display "f ∘ g: ") (display f-after-g) (newline)
(display "g ∘ f: ") (display g-after-f) (newline)

;; Compare with identities
(define id-x (category-identity x))
(define id-y (category-identity y))

(display "id_X: ") (display id-x) (newline)
(display "id_Y: ") (display id-y) (newline)

;; In a complete implementation, we would check if f-after-g equals id-y
;; and g-after-f equals id-x
