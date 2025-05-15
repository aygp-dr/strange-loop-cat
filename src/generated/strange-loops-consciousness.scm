;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; A simplified model of consciousness as a strange loop
(display "A Simplified Model of Self-Awareness:\n")

;; Define a system that has:
;; 1. A base level of "perceptions"
;; 2. A model of itself
;; 3. The ability to model its own modeling

;; Perceptions (base level data)
(define perceptions '(red circle large moving))

;; A simple model: categorize perceptions
(define (create-model perceptions)
  (list 'model
        (list 'color (find (lambda (p) (member p '(red green blue))) perceptions))
        (list 'shape (find (lambda (p) (member p '(circle square triangle))) perceptions))
        (list 'size (find (lambda (p) (member p '(large medium small))) perceptions))
        (list 'motion (find (lambda (p) (member p '(moving stationary))) perceptions))))

;; Create model from perceptions
(define model (create-model perceptions))

(display "\nBase perceptions: ") (display perceptions) (newline)
(display "Model of perceptions: ") (display model) (newline)

;; Self-awareness: the system models itself modeling
(define (create-self-model base-model)
  (list 'self-model
        (list 'modeling-activity 'active)
        (list 'current-model base-model)
        (list 'model-complexity (length base-model))
        (list 'aware-of-modeling #t)))

;; Create self-model
(define self-model (create-self-model model))

(display "\nSelf-model (modeling itself modeling): ") 
(display self-model) (newline)

;; The strange loop: The system can update its perceptions to include awareness of itself
(define updated-perceptions 
  (append perceptions 
          (list 'self-aware 'modeling)))

(display "\nUpdated perceptions (including self-awareness): ") 
(display updated-perceptions) (newline)

;; And can now model those new perceptions, including the perception of itself
(define updated-model (create-model updated-perceptions))
(display "Updated model (including modeling itself): ") 
(display updated-model) (newline)

(display "\nThis demonstrates a (simplified) strange loop where the system's model\n")
(display "of itself becomes part of what it models, creating a self-referential loop.\n")
(display "Hofstadter argues consciousness emerges from this kind of self-reference.\n")
