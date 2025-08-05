;; test-runner.scm - Simple test framework for our category theory implementation

(define-module (test-runner)
  #:use-module (category)
  #:export (run-all-tests))

(define (run-all-tests)
  (display "Running category theory tests...\n")
  
  ;; Test basic category creation
  (let* ((a (make-object "A"))
         (b (make-object "B"))
         (c (make-object "C"))
         (f (make-morphism "f" a b))
         (g (make-morphism "g" b c))
         (cat (make-category "Example" (list a b c) (list f g))))
    
    (display "Created a category with 3 objects and 2 morphisms\n")
    
    ;; Test composition
    (let ((g-f (category-compose f g)))
      (display "Tested morphism composition: ")
      (display (cadr g-f))
      (newline))
    
    ;; Test identity
    (let ((id-a (category-identity a)))
      (display "Created identity morphism: ")
      (display (cadr id-a))
      (newline)))
  
  (display "All tests completed.\n"))
