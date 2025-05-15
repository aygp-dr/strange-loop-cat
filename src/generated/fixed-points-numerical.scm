(use-modules (ice-9 format)
             (srfi srfi-1)  ; List library
             (srfi srfi-11)) ; let-values

;; Fixed point methods for solving equations

;; Newton's method as fixed point iteration
(define* (newton-method f df x0 
                       #:optional
                       (tolerance 0.00001)
                       (max-iterations 100))
  ;; Transform the equation f(x) = 0 into a fixed point problem
  ;; x = x - f(x)/f'(x)
  (let ((g (lambda (x) (- x (/ (f x) (df x))))))
    (fixed-point g x0 
                #:tolerance tolerance
                #:max-iterations max-iterations)))

;; Example: Find square root of 2 using Newton's method
(define (f-sqrt2 x) (- (* x x) 2))
(define (df-sqrt2 x) (* 2 x))

(format #t "Computing √2 using Newton's method:\n")
(let-values (((result status) 
              (newton-method f-sqrt2 df-sqrt2 1.0)))
  (format #t "√2 ≈ ~a (status: ~a)\n" result status)
  (format #t "Verification: ~a² = ~a\n" 
          result (* result result)))

;; Method of successive approximations for numerical integration
(define* (successive-approximation-integral f a b initial
                                          #:optional
                                          (steps 100)
                                          (tolerance 0.00001))
  ;; The integration operator as a fixed point problem
  (let ((step (/ (- b a) steps))
        (integration-operator 
         (lambda (g)
           (lambda (x)
             (if (< x a)
                 initial
                 (let ((x-prev (- x step)))
                   (+ (g x-prev)
                      (* step (f x-prev (g x-prev))))))))))
    
    ;; Apply fixed point iteration on functions
    (let ((result-function
           (fixed-point integration-operator
                       (lambda (x) initial)
                       #:tolerance tolerance)))
      ;; Return the final value at point b
      (result-function b))))

;; This gives us a framework for solving differential equations
;; as fixed point problems
