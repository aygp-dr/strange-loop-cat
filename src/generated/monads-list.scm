;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; The List monad in the category of sets
;; For an object (set) A, List(A) = all possible lists of elements from A
;; For a morphism f: A → B, List(f) maps each list [a1, a2, ...] to [f(a1), f(a2), ...]

;; Unit: A → List(A)
;; Maps an element to a singleton list containing that element
(define (unit-list x)
  (list x))

;; Join: List(List(A)) → List(A)
;; Flattens a list of lists into a single list
(define (join-list llx)
  (apply append llx))

;; Bind operation (derived from unit and join)
(define (bind-list xs f)
  (join-list (map f xs)))

;; Example usage:
(display "List Monad Examples:\n")

(display "\nUnit examples:\n")
(display "unit(5) = ") (display (unit-list 5)) (newline)
(display "unit(\"hello\") = ") (display (unit-list "hello")) (newline)

(display "\nJoin examples:\n")
(display "join([[1, 2], [3, 4]]) = ") (display (join-list '((1 2) (3 4)))) (newline)
(display "join([[1], [2, 3], []]) = ") (display (join-list '((1) (2 3) ()))) (newline)

;; Define some functions for the bind examples
(define (sqrt-list x)
  (list (sqrt x) (- (sqrt x))))

(define (add1-list x)
  (list (+ x 1) (- x 1)))

(display "\nBind examples:\n")
(display "bind([4], sqrt-list) = ") (display (bind-list (list 4) sqrt-list)) (newline)
(display "bind([1, 4, 9], sqrt-list) = ") (display (bind-list (list 1 4 9) sqrt-list)) (newline)
(display "bind([5], add1-list) = ") (display (bind-list (list 5) add1-list)) (newline)

;; Chain of computations with bind
(display "\nChained computation:\n")
(define result 
  (bind-list (list 4) 
             (lambda (x) 
               (bind-list (sqrt-list x)
                         add1-list))))
(display "[4] → [2, -2] → [3, 1, -1, -3] = ") (display result) (newline)
