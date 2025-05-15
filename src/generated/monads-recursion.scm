;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; We'll use the List monad from earlier to implement a recursive search

;; Unit: A → List(A)
(define (unit-list x)
  (list x))

;; Join: List(List(A)) → List(A)
(define (join-list llx)
  (apply append llx))

;; Bind operation (derived from unit and join)
(define (bind-list xs f)
  (join-list (map f xs)))

;; Graph represented as an adjacency list
(define graph
  '((A . (B C))
    (B . (D))
    (C . (D E))
    (D . (F))
    (E . (F))
    (F . (A))))  ; Note the loop back to A

;; Get neighbors of a node
(define (neighbors node)
  (let ((pair (assoc node graph)))
    (if pair
        (cdr pair)
        '())))

;; Search for paths up to a certain depth
(define (search start depth)
  (if (<= depth 0)
      (unit-list (list start))
      (bind-list (unit-list (list start))
                 (lambda (path)
                   (let ((last-node (car path)))
                     (bind-list (neighbors last-node)
                                (lambda (next-node)
                                  (if (member next-node path)
                                      '() ; Skip if we've already visited this node
                                      (bind-list (search next-node (- depth 1))
                                                 (lambda (subpath)
                                                   (unit-list 
                                                    (cons last-node subpath))))))))))))

;; Find all paths of length 4 starting from node A
(display "Searching for paths in a graph with a loop:\n")
(let ((paths (search 'A 5)))
  (for-each (lambda (path)
              (display path) (newline))
            paths))

;; This demonstrates a strange loop pattern:
;; - The graph itself contains a loop (F → A)
;; - The recursion in 'search' is itself a kind of loop
;; - The monad abstracts and structures this looping behavior
(display "\nNotice how some paths form loops, returning to node A.\n")
(display "This is a computational demonstration of a strange loop!\n")
