;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; Define a simple system of symbols and rules (a kind of formal system)
(display "A Self-Referential Symbol System:\n")

;; Define our alphabet of symbols
(define symbols '(a b c d))

;; Rules for transforming symbols (simplified production rules)
(define (apply-rule symbol)
  (case symbol
    ((a) 'b)
    ((b) 'c)
    ((c) 'd)
    ((d) 'a)
    (else symbol)))

;; Function to apply rules to a string of symbols
(define (apply-rules-to-string str)
  (map apply-rule str))

;; Create a simple string
(define initial-string '(a b a c))

;; Show multiple applications of the rules
(display "\nInitial string: ") (display initial-string) (newline)

(define step-1 (apply-rules-to-string initial-string))
(display "After 1 step: ") (display step-1) (newline)

(define step-2 (apply-rules-to-string step-1))
(display "After 2 steps: ") (display step-2) (newline)

(define step-3 (apply-rules-to-string step-2))
(display "After 3 steps: ") (display step-3) (newline)

(define step-4 (apply-rules-to-string step-3))
(display "After 4 steps: ") (display step-4) (newline)

;; Demonstrate self-reference by encoding the rules in the system itself
(display "\nSelf-Reference in the System:\n")

;; Encode the rules as a string: ((a b) (b c) (c d) (d a))
(define rule-encoding '((a b) (b c) (c d) (d a)))

;; A meta-rule that reads rule encodings and applies them
(define (apply-meta-rule encoded-rules symbol)
  (let ((matching-rule (find (lambda (rule) (eq? (car rule) symbol)) 
                             encoded-rules)))
    (if matching-rule
        (cadr matching-rule)
        symbol)))

;; Apply meta-rules to the original string
(define (apply-meta-rules encoded-rules str)
  (map (lambda (symbol) (apply-meta-rule encoded-rules symbol)) str))

;; Show that the meta-system produces the same results
(display "Using meta-rules with encoded ruleset:\n")
(define meta-step-1 (apply-meta-rules rule-encoding initial-string))
(display "After 1 step: ") (display meta-step-1) (newline)

(define meta-step-2 (apply-meta-rules rule-encoding meta-step-1))
(display "After 2 steps: ") (display meta-step-2) (newline)

;; The strange loop: we can apply the meta-system to the rule encoding itself
(display "\nStrange Loop: Applying system to its own rules:\n")
(display "Original rules: ") (display rule-encoding) (newline)

;; Apply the transformation to the first element of each rule
(define transformed-rules
  (map (lambda (rule)
         (list (apply-meta-rule rule-encoding (car rule))
               (cadr rule)))
       rule-encoding))

(display "Transformed rules: ") (display transformed-rules) (newline)

(display "\nThis demonstrates a strange loop because the system can\n")
(display "operate on its own rules, creating a self-referential structure.\n")
