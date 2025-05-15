;; Maybe monad implementation for handling possible failures

;; Maybe monad constructors
(define (just x) (cons 'just x))
(define (nothing) 'nothing)

;; Maybe monad operations
(define (maybe-return x) (just x))

(define (maybe-bind m f)
  (if (eq? m 'nothing)
      'nothing
      (f (cdr m))))

;; Helper functions
(define (maybe-value m default)
  (if (eq? m 'nothing)
      default
      (cdr m)))

(define (is-just? m)
  (and (pair? m) (eq? (car m) 'just)))

(define (is-nothing? m)
  (eq? m 'nothing))

;; Factorial with Maybe monad to handle invalid inputs
(define (safe-factorial n)
  ;; First check if n is a valid input
  (if (or (not (integer? n)) (< n 0))
      (nothing)  ;; Return nothing for invalid inputs
      (let loop ((n n) 
                 (acc (maybe-return 1)))
        (if (<= n 1)
            acc
            (maybe-bind acc
                       (lambda (result)
                         (maybe-return (* result n))))
            (loop (- n 1))))))

;; Test cases
(display "Factorial with Maybe monad:\n")

(define valid-result (safe-factorial 5))
(display "safe-factorial 5: ")
(if (is-just? valid-result)
    (display (maybe-value valid-result 'error))
    (display "Error: Invalid input"))
(newline)

(define negative-result (safe-factorial -3))
(display "safe-factorial -3: ")
(if (is-just? negative-result)
    (display (maybe-value negative-result 'error))
    (display "Error: Invalid input"))
(newline)

(define non-integer-result (safe-factorial 3.5))
(display "safe-factorial 3.5: ")
(if (is-just? non-integer-result)
    (display (maybe-value non-integer-result 'error))
    (display "Error: Invalid input"))
(newline)
