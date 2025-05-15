;; Y combinator (fixed-point combinator)
(define (Y f)
  ((lambda (x) (f (lambda (y) ((x x) y))))
   (lambda (x) (f (lambda (y) ((x x) y))))))

;; Using the Y combinator to create a factorial function
(define factorial
  (Y (lambda (f)
       (lambda (n)
         (if (zero? n)
             1
             (* n (f (- n 1))))))))

;; Test it
(display "Factorial of 5: ")
(display (factorial 5))
(newline)
