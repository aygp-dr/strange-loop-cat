(use-modules (ice-9 format)
             (srfi srfi-1)     ; List library
             (srfi srfi-11)    ; let-values 
             (srfi srfi-26)    ; Cut/cute partial application
             (srfi srfi-42)    ; Eager comprehensions
             (ice-9 match))    ; Pattern matching

(display "Modules loaded successfully.\n")

;; Continuous fixed point finder
(define* (fixed-point f initial-guess #:optional
                      (tolerance 0.00001)
                      (max-iterations 1000))
  (let loop ((guess initial-guess)
             (iterations 0))
    (let ((next (f guess)))
      (cond
       ((> iterations max-iterations)
        (values guess 'max-iterations-reached))
       
       ((< (abs (- next guess)) tolerance)
        (values guess 'converged))
       
       (else
        (loop next (+ iterations 1)))))))

;; For discrete cases (exact values)
(define (discrete-fixed-point f initial-value)
  (let loop ((current initial-value)
             (iterations 0))
    (let ((next (f current)))
      (cond
       ((> iterations 1000)
        (values current 'max-iterations-reached))
       
       ((equal? next current)
        (values current 'converged))
       
       (else
        (loop next (+ iterations 1)))))))

;; Test the fixed point finder
(let-values (((result status) (fixed-point cos 1.0)))
  (format #t "Fixed point of cosine: ~a (status: ~a)\n" 
          result status))
