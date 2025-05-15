;; Functions for verifying and applying the Banach Fixed Point Theorem

;; Generate a sample of points from a domain
(define (sample-domain lower upper samples)
  (let ((step (/ (- upper lower) (- samples 1))))
    (map (lambda (i) (+ lower (* i step)))
         (iota samples))))

;; Compute Lipschitz constant approximation
(define (approximate-lipschitz-constant f domain-min domain-max samples)
  (let* ((points (sample-domain domain-min domain-max samples))
         (point-pairs (append-map 
                       (lambda (x)
                         (map (lambda (y) (cons x y))
                              (remove (lambda (z) (= x z)) points)))
                       points))
         (ratios 
          (map (lambda (pair)
                 (let ((x (car pair))
                       (y (cdr pair)))
                   (/ (abs (- (f x) (f y)))
                      (abs (- x y)))))
               point-pairs)))
    (apply max ratios)))

;; Check if a function is a contraction mapping
(define (contraction-mapping? f domain-min domain-max samples)
  (< (approximate-lipschitz-constant f domain-min domain-max samples) 1.0))

;; Apply Banach theorem to find fixed point
(define* (banach-fixed-point f domain-min domain-max
                           #:optional
                           (samples 50)
                           (initial-guess (/ (+ domain-min domain-max) 2))
                           (tolerance 0.00001))
  
  ;; First check if the function is a contraction mapping
  (let ((lipschitz (approximate-lipschitz-constant 
                    f domain-min domain-max samples)))
    
    (if (>= lipschitz 1.0)
        (begin
          (format #t "Warning: Function has Lipschitz constant ~a ≥ 1\n" 
                  lipschitz)
          (format #t "Convergence not guaranteed by Banach theorem\n")
          (fixed-point f initial-guess #:tolerance tolerance))
        
        (begin
          (format #t "Function has Lipschitz constant ~a < 1\n" lipschitz)
          (format #t "Convergence guaranteed by Banach Fixed Point Theorem\n")
          ;; We can predict the number of iterations needed for convergence
          (let* ((error-reduction-per-step lipschitz)
                 (initial-error (abs (- (f initial-guess) initial-guess)))
                 (iterations-needed
                  (ceiling (/ (log (/ tolerance initial-error))
                              (log error-reduction-per-step)))))
            (format #t "Predicted iterations needed: ~a\n" iterations-needed)
            (fixed-point f initial-guess #:tolerance tolerance))))))

;; Example with guaranteed convergence: f(x) = cos(x)
(format #t "\nTesting Banach theorem with cos(x):\n")
(banach-fixed-point cos 0.0 1.0)

;; Example without guaranteed convergence: f(x) = 3x(1-x)
(format #t "\nTesting with logistic map f(x) = 3x(1-x):\n")
(banach-fixed-point 
 (lambda (x) (* 3 x (- 1 x)))
 0.0 1.0)
