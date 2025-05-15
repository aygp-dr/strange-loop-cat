;; Fixed point operator for numeric approximation
(define (fixed-point f start-value)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try start-value))

;; Example: Computing square root using fixed point
(define (sqrt-fixed-point x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
               1.0))

(display "Square root of 16 using fixed point: ")
(display (sqrt-fixed-point 16))
(newline)

;; Solving equations using fixed points
(display "Fixed point of cos(x): ")
(display (fixed-point cos 1.0))
(newline)

;; Newton's method as fixed point iteration
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derivative g) x)))))

(define (derivative g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

;; Find roots of x^2 - 4 = 0 using Newton's method
(define (square-minus-4 x)
  (- (* x x) 4))

(display "Root of x^2 - 4 = 0 using Newton's method: ")
(display (newton-method square-minus-4 1.0))
(newline)
