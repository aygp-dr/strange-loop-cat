;; Gradient descent with convergence tracking
(define (gradient-descent f gradient initial-point
                         #:optional #:key
                         (learning-rate 0.01)
                         (tolerance 0.00001)
                         (max-iterations 1000))
  
  ;; Track history of points, values, and gradients
  (define history '())
  
  ;; Check for oscillation in recent iterations
  (define (oscillating? history-subset)
    (let* ((gradients (map caddr history-subset))
           (signs (map (lambda (g) (if (> g 0) 1 -1)) gradients))
           (sign-changes 
            (count (lambda (pair) (not (= (car pair) (cadr pair))))
                   (zip signs (cdr signs)))))
      (> sign-changes (/ (length signs) 2))))
  
  ;; Check if progress is too slow
  (define (progress-too-slow? history-subset)
    (let* ((values (map cadr history-subset))
           (improvements 
            (map (lambda (pair) (abs (- (car pair) (cadr pair))))
                 (zip values (cdr values)))))
      (< (apply max improvements) (* tolerance 10))))
  
  ;; Main iteration loop
  (let loop ((point initial-point)
             (iteration 0))
    (let* ((value (f point))
           (grad (gradient point))
           (grad-magnitude (sqrt (apply + (map * grad grad)))))
      
      ;; Add current state to history
      (set! history (cons (list point value grad) history))
      
      (cond
       ;; Stop if maximum iterations reached
       ((>= iteration max-iterations)
        (values point value 'max-iterations-reached history))
       
       ;; Stop if gradient is nearly zero (at fixed point)
       ((< grad-magnitude tolerance)
        (values point value 'converged-to-fixed-point history))
       
       ;; Stop if oscillating (might need smaller learning rate)
       ((and (> (length history) 10)
             (oscillating? (take history 10)))
        (values point value 'oscillation-detected history))
       
       ;; Stop if progress is too slow
       ((and (> (length history) 5)
             (progress-too-slow? (take history 5)))
        (values point value 'slow-progress history))
       
       ;; Continue with gradient descent step
       (else
        (let ((next-point 
               (map (lambda (p g) (- p (* learning-rate g)))
                    point grad)))
          (loop next-point (+ iteration 1))))))))

;; Example usage with a simple function
(define (example-function point)
  (let ((x (car point))
        (y (cadr point)))
    (+ (* x x) (* y y)))) ; f(x,y) = x² + y²

(define (example-gradient point)
  (let ((x (car point))
        (y (cadr point)))
    (list (* 2 x) (* 2 y)))) ; ∇f = (2x, 2y)

;; Test the gradient descent
(let-values (((minimum value status history) 
              (gradient-descent example-function 
                               example-gradient
                               '(10.0 10.0))))
  (format #t "Minimum found at: (~a, ~a)\n" 
          (car minimum) (cadr minimum))
  (format #t "Minimum value: ~a\n" value)
  (format #t "Status: ~a\n" status)
  (format #t "Iterations: ~a\n" (length history)))
