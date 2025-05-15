;; Generate data for plotting fixed point iterations
(define (generate-iteration-data f initial iterations)
  (let loop ((x initial)
             (i 0)
             (results '()))
    (if (>= i iterations)
        (reverse results)
        (let ((next (f x)))
          (loop next
                (+ i 1)
                (cons (list i x next) results))))))

;; Example with cosine function
(define cos-iterations (generate-iteration-data cos 1.0 20))

;; Output data in a format suitable for plotting
(format #t "# Iterations of cos(x) starting at x₀ = 1.0\n")
(format #t "# i, x_i, cos(x_i)\n")
(for-each (lambda (row)
            (format #t "~a, ~a, ~a\n" 
                    (car row) (cadr row) (caddr row)))
          cos-iterations)
