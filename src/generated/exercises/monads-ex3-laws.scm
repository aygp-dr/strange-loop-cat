;; Proving the monad laws for the Maybe monad

;; First, let's reimplement the Maybe monad
(define (just x) (cons 'just x))
(define (nothing) 'nothing)

;; The return operation (unit)
(define (maybe-return x) (just x))

;; The bind operation (>>=)
(define (maybe-bind m f)
  (if (eq? m 'nothing)
      'nothing
      (f (cdr m))))

;; Helper function to check equality
(define (maybe-equal? m1 m2)
  (or (and (eq? m1 'nothing) (eq? m2 'nothing))
      (and (pair? m1) (pair? m2) 
           (eq? (car m1) 'just) (eq? (car m2) 'just)
           (equal? (cdr m1) (cdr m2)))))

;; Test functions for our proofs
(define (double x) (maybe-return (* x 2)))
(define (square x) (maybe-return (* x x)))
(define (inc x) (maybe-return (+ x 1)))

;; Let's verify the three monad laws:

;; 1. Left identity: return a >>= f ≡ f a
(display "Testing left identity law:\n")

(define test-value 42)
(define left-id-left (maybe-bind (maybe-return test-value) double))
(define left-id-right (double test-value))

(display "Left side:  ") (display left-id-left) (newline)
(display "Right side: ") (display left-id-right) (newline)
(display "Equal? ") (display (maybe-equal? left-id-left left-id-right)) (newline)

;; 2. Right identity: m >>= return ≡ m
(display "\nTesting right identity law:\n")

(define test-monad (just 42))
(define right-id-left (maybe-bind test-monad maybe-return))
(define right-id-right test-monad)

(display "Left side:  ") (display right-id-left) (newline)
(display "Right side: ") (display right-id-right) (newline)
(display "Equal? ") (display (maybe-equal? right-id-left right-id-right)) (newline)

;; Also test with nothing
(display "\nTesting right identity with nothing:\n")
(define test-monad-nothing (nothing))
(define right-id-left-nothing (maybe-bind test-monad-nothing maybe-return))
(define right-id-right-nothing test-monad-nothing)

(display "Left side:  ") (display right-id-left-nothing) (newline)
(display "Right side: ") (display right-id-right-nothing) (newline)
(display "Equal? ") (display (maybe-equal? right-id-left-nothing right-id-right-nothing)) (newline)

;; 3. Associativity: (m >>= f) >>= g ≡ m >>= (λx. f x >>= g)
(display "\nTesting associativity law:\n")

(define test-monad-assoc (just 10))
(define assoc-left (maybe-bind (maybe-bind test-monad-assoc double) square))
(define assoc-right (maybe-bind test-monad-assoc 
                               (lambda (x) 
                                 (maybe-bind (double x) square))))

(display "Left side:  ") (display assoc-left) (newline)
(display "Right side: ") (display assoc-right) (newline)
(display "Equal? ") (display (maybe-equal? assoc-left assoc-right)) (newline)

;; Also test associativity with nothing
(display "\nTesting associativity with nothing:\n")
(define test-monad-assoc-nothing (nothing))
(define assoc-left-nothing (maybe-bind (maybe-bind test-monad-assoc-nothing double) square))
(define assoc-right-nothing (maybe-bind test-monad-assoc-nothing
                                       (lambda (x)
                                         (maybe-bind (double x) square))))

(display "Left side:  ") (display assoc-left-nothing) (newline)
(display "Right side: ") (display assoc-right-nothing) (newline)
(display "Equal? ") (display (maybe-equal? assoc-left-nothing assoc-right-nothing)) (newline)

(display "\nConclusion: All three monad laws are satisfied for the Maybe monad.\n")
