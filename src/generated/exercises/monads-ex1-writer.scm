;; Writer Monad Implementation
;; The Writer monad allows us to accumulate information (like logs) during computation

;; First include necessary utilities and modules
(add-to-load-path "../src/guile")
(use-modules (ice-9 format))

;; Writer monad constructor: creates a pair of value and log
(define (writer-return value)
  (cons value '()))

;; Writer monad bind: sequences operations while accumulating logs
(define (writer-bind m f)
  (let* ((value (car m))
         (log1 (cdr m))
         (result (f value))
         (value2 (car result))
         (log2 (cdr result)))
    (cons value2 (append log1 log2))))

;; Helper function to add a log entry
(define (tell message)
  (cons #f (list message)))

;; Helper function to run a writer computation
(define (run-writer m)
  m)

;; Helper function to get the final value of a writer computation
(define (writer-value m)
  (car m))

;; Helper function to get the logs from a writer computation
(define (writer-logs m)
  (cdr m))

;; Helper to create a writer from a value and a log entry
(define (writer value message)
  (cons value (list message)))

;; Example: Factorial with logging
(define (factorial-with-logging n)
  (let loop ((n n)
             (result (writer 1 (format #f "Starting factorial calculation for ~a" n))))
    (if (<= n 1)
        (writer-bind result 
                    (lambda (r) 
                      (writer r (format #f "Finished factorial calculation with result ~a" r))))
        (writer-bind result
                    (lambda (r)
                      (let ((new-result (* r n)))
                        (writer new-result 
                                (format #f "Multiplied by ~a to get ~a" n new-result))))
                    loop (- n 1)))))

;; Test the Writer monad
(define result (factorial-with-logging 5))
(display "Final value: ") (display (writer-value result)) (newline)
(display "Logs:\n")
(for-each (lambda (log) (display "- ") (display log) (newline))
          (writer-logs result))
