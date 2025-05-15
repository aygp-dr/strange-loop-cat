;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; The State monad represents computations that maintain state
;; For a state type S and value type A:
;; State_S(A) = S → (A × S)
;; That is, a state computation takes an initial state and returns a value and a new state

;; We'll represent state computations as functions: s → (list value new-state)

;; Unit: A → State_S(A)
;; Returns the value without changing the state
(define (unit-state x)
  (lambda (s) (list x s)))

;; Bind operation for State monad
(define (bind-state computation f)
  (lambda (s)
    (let* ((result (computation s))
           (value (car result))
           (new-state (cadr result))
           (new-computation (f value)))
      (new-computation new-state))))

;; Some helper functions for state manipulation
(define (get-state)
  (lambda (s) (list s s)))

(define (put-state new-s)
  (lambda (_) (list #t new-s)))

(define (modify-state f)
  (lambda (s) (list #t (f s))))

;; Example: A counter with operations to increment, get, and reset
(display "State Monad Example: Counter\n")

;; Increment the counter
(define (increment)
  (bind-state (get-state)
              (lambda (count)
                (put-state (+ count 1)))))

;; Reset the counter
(define (reset)
  (put-state 0))

;; Get the current counter value
(define (get-counter)
  (get-state))

;; A sequence of counter operations
(define counter-ops
  (bind-state (reset)
              (lambda (_)
                (bind-state (increment)
                            (lambda (_)
                              (bind-state (increment)
                                          (lambda (_)
                                            (bind-state (get-counter)
                                                        (lambda (count)
                                                          (unit-state (format #f "Counter value: ~a" count)))))))))))

;; Run the operations starting with an initial state of 0
(display "\nRunning counter operations:\n")
(display (car (counter-ops 0)))
(newline)

;; A more complex example: factorial calculator with logging
(display "\nState Monad Example: Factorial with Logging\n")

;; Calculate factorial and log the steps
(define (factorial-with-log n)
  (if (<= n 1)
      (bind-state 
       (modify-state (lambda (log) (cons (format #f "factorial(~a) = 1" n) log)))
       (lambda (_) (unit-state 1)))
      (bind-state
       (modify-state (lambda (log) (cons (format #f "Computing factorial(~a)..." n) log)))
       (lambda (_)
         (bind-state
          (factorial-with-log (- n 1))
          (lambda (fact-n-1)
            (let ((result (* n fact-n-1)))
              (bind-state
               (modify-state (lambda (log) (cons (format #f "factorial(~a) = ~a" n result) log)))
               (lambda (_) (unit-state result))))))))))

;; Run factorial with empty log
(display "\nFactorial of 4 with logging:\n")
(let* ((result ((factorial-with-log 4) '()))
       (factorial-value (car result))
       (logs (reverse (cadr result))))  ; Reverse to get chronological order
  (display "Result: ") (display factorial-value) (newline)
  (display "Logs:\n")
  (for-each (lambda (log) (display "  ") (display log) (newline)) logs))
