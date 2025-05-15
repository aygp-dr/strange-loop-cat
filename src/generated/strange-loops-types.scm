;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a simple category
(define a (make-object "A"))
(define b (make-object "B"))
(define c (make-object "C"))
(define f-ab (make-morphism "f_AB" a b))
(define f-bc (make-morphism "f_BC" b c))
(define f-ca (make-morphism "f_CA" c a))

;; This forms a loop: A → B → C → A
(display "Category with a loop structure:\n")
(display "Objects: A, B, C\n")
(display "Morphisms: f_AB: A → B, f_BC: B → C, f_CA: C → A\n")

;; Define a simple endofunctor that rotates the objects
(define (rotate-functor obj)
  (cond ((equal? obj a) b)
        ((equal? obj b) c)
        ((equal? obj c) a)
        (else (error "Object not in category"))))

;; Applying the functor twice gives a different object
(display "\nApplying rotate-functor once:\n")
(display "F(A) = ") (display (rotate-functor a)) (newline)
(display "F(B) = ") (display (rotate-functor b)) (newline)
(display "F(C) = ") (display (rotate-functor c)) (newline)

;; Applying the functor three times gives the original object (fixed point)
(display "\nApplying rotate-functor three times (composing with itself twice):\n")
(define (rotate-3-times obj)
  (rotate-functor (rotate-functor (rotate-functor obj))))

(display "F³(A) = ") (display (rotate-3-times a)) (newline)
(display "F³(B) = ") (display (rotate-3-times b)) (newline)
(display "F³(C) = ") (display (rotate-3-times c)) (newline)

;; The Y combinator implementation
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;; Using the Y combinator to implement factorial
(define factorial
  (Y (lambda (f)
       (lambda (n)
         (if (= n 0)
             1
             (* n (f (- n 1))))))))

(display "\nFactorial function implemented using Y combinator:\n")
(display "factorial(5) = ") (display (factorial 5)) (newline)
