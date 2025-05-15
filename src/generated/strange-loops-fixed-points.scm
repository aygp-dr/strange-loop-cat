;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a simple list type recursively
;; List(A) = 1 + A × List(A)  (1 represents the empty list)
(display "Defining a recursive list type:\n")
(display "List(A) = 1 + A × List(A)\n")

;; Simple implementation of lists
(define (empty-list? lst) (null? lst))
(define (cons-list hd tl) (cons hd tl))
(define (head-list lst) (car lst))
(define (tail-list lst) (cdr lst))

;; Create a list
(define sample-list (cons-list 1 (cons-list 2 (cons-list 3 '()))))

(display "\nExample list: ") (display sample-list) (newline)
(display "Head: ") (display (head-list sample-list)) (newline)
(display "Tail: ") (display (tail-list sample-list)) (newline)

;; Define a binary tree type recursively
;; Tree(A) = 1 + A × Tree(A) × Tree(A)  (1 represents the empty tree)
(display "\nDefining a recursive binary tree type:\n")
(display "Tree(A) = 1 + A × Tree(A) × Tree(A)\n")

;; Simple implementation of binary trees
(define (make-empty-tree) '())
(define (make-tree value left right) (list value left right))
(define (empty-tree? tree) (null? tree))
(define (tree-value tree) (car tree))
(define (tree-left tree) (cadr tree))
(define (tree-right tree) (caddr tree))

;; Create a tree
(define sample-tree 
  (make-tree 1
             (make-tree 2 (make-empty-tree) (make-empty-tree))
             (make-tree 3 (make-empty-tree) (make-empty-tree))))

(display "\nTree values (flattened): ")
(define (tree-values tree)
  (if (empty-tree? tree)
      '()
      (append (list (tree-value tree))
              (tree-values (tree-left tree))
              (tree-values (tree-right tree)))))

(display (tree-values sample-tree)) (newline)

;; Iterative fixed point finder for numeric functions
(define (find-fixed-point f initial-guess max-iterations)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  
  (define (iterate guess iteration)
    (if (= iteration max-iterations)
        guess
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (iterate next (+ iteration 1))))))
  
  (iterate initial-guess 0))

;; Example functions with fixed points
(display "Fixed Points of Functions:\n")

;; cos(x) has a fixed point around 0.739
(display "\n1. Fixed point of cos(x):\n")
(define cos-fixed-point (find-fixed-point cos 1.0 100))
(display "   x = cos(x) ≈ ") (display cos-fixed-point) (newline)
(display "   Verification: cos(") (display cos-fixed-point) 
(display ") = ") (display (cos cos-fixed-point)) (newline)

;; √x has fixed points at 0 and 1
(define (sqrt-function x) (sqrt x))
(display "\n2. Fixed points of √x:\n")
(define sqrt-fixed-point-1 (find-fixed-point sqrt-function 0.5 100))
(display "   x = √x ≈ ") (display sqrt-fixed-point-1) (newline)
(display "   Verification: √") (display sqrt-fixed-point-1) 
(display " = ") (display (sqrt sqrt-fixed-point-1)) (newline)

;; x² has fixed points at 0 and 1
(define (square x) (* x x))
(display "\n3. Fixed points of x²:\n")
(define square-fixed-point-1 (find-fixed-point square 0.5 100))
(display "   x = x² ≈ ") (display square-fixed-point-1) (newline)
(display "   Verification: ") (display square-fixed-point-1) 
(display "² = ") (display (square square-fixed-point-1)) (newline)

(define square-fixed-point-2 (find-fixed-point square 1.5 100))
(display "   Another fixed point x = x² ≈ ") (display square-fixed-point-2) (newline)
(display "   Verification: ") (display square-fixed-point-2) 
(display "² = ") (display (square square-fixed-point-2)) (newline)

(display "\nConnection to strange loops:\n")
(display "Fixed points create self-reference where the output of a\n")
(display "transformation equals its input, forming a perfect loop.\n")
