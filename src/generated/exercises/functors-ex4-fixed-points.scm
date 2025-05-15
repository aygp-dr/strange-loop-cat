;; Fixed point calculations for more complex examples

;; Basic fixed point function from previous examples
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

;; Matrix operations (simplified for 2x2 matrices)
;; Represent a 2x2 matrix as a list of lists
(define (matrix-multiply m1 m2)
  (list 
   (list 
    (+ (* (car (car m1)) (car (car m2))) 
       (* (cadr (car m1)) (car (cadr m2))))
    (+ (* (car (car m1)) (cadr (car m2))) 
       (* (cadr (car m1)) (cadr (cadr m2)))))
   (list 
    (+ (* (car (cadr m1)) (car (car m2))) 
       (* (cadr (cadr m1)) (car (cadr m2))))
    (+ (* (car (cadr m1)) (cadr (car m2))) 
       (* (cadr (cadr m1)) (cadr (cadr m2)))))))

(define (matrix-scalar-multiply scalar matrix)
  (list 
   (list (* scalar (car (car matrix))) 
         (* scalar (cadr (car matrix))))
   (list (* scalar (car (cadr matrix))) 
         (* scalar (cadr (cadr matrix))))))

(define (matrix-add m1 m2)
  (list 
   (list (+ (car (car m1)) (car (car m2)))
         (+ (cadr (car m1)) (cadr (car m2))))
   (list (+ (car (cadr m1)) (car (cadr m2)))
         (+ (cadr (cadr m1)) (cadr (cadr m2))))))

(define (matrix-subtract m1 m2)
  (matrix-add m1 (matrix-scalar-multiply -1 m2)))

;; Matrix normalization (divide by largest element)
(define (matrix-normalize m)
  (let ((max-element (apply max (append (car m) (cadr m)))))
    (if (= max-element 0)
        m  ;; Avoid division by zero
        (matrix-scalar-multiply (/ 1.0 max-element) m))))

;; Power iteration method for finding the dominant eigenvalue and eigenvector
(define (power-iteration matrix initial-vector max-iterations)
  (define (iterate vector iterations)
    (if (= iterations 0)
        vector
        (let ((new-vector (matrix-multiply matrix vector)))
          (iterate (matrix-normalize new-vector) (- iterations 1)))))
  
  (iterate initial-vector max-iterations))

;; Example matrix with known eigenvalues
(define test-matrix '((4 1) (2 3)))
(define initial-vector '((1) (1)))

;; Display the matrix
(display "Finding eigenvalues of matrix:\n")
(display test-matrix)
(newline)

;; Run power iteration
(display "\nPower iteration to find dominant eigenvector:\n")
(define eigenvector (power-iteration test-matrix initial-vector 10))
(display "Approximate eigenvector: ")
(display eigenvector)
(newline)

;; Estimate eigenvalue using the Rayleigh quotient
(define Ax (matrix-multiply test-matrix eigenvector))
(define x-dot-Ax (+ (* (car (car eigenvector)) (car (car Ax)))
                    (* (car (cadr eigenvector)) (car (cadr Ax)))))
(define x-dot-x (+ (* (car (car eigenvector)) (car (car eigenvector)))
                   (* (car (cadr eigenvector)) (car (cadr eigenvector)))))
(define eigenvalue (/ x-dot-Ax x-dot-x))

(display "Approximate dominant eigenvalue: ")
(display eigenvalue)
(newline)

;; The theoretical eigenvalues of the matrix are 5 and 2
(display "\nTheoretical eigenvalues for comparison: 5 and 2\n")
