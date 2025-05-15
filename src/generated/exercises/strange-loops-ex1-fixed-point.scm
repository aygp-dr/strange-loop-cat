;; Fixed Point Combinator Exercise

;; Y combinator implementation
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;; Using Y combinator to define factorial without explicit recursion
(define factorial
  (Y (lambda (fact)
       (lambda (n)
         (if (= n 0)
             1
             (* n (fact (- n 1))))))))

;; Test factorial
(display "Factorial using Y combinator:\n")
(display "factorial(0) = ") (display (factorial 0)) (newline)
(display "factorial(1) = ") (display (factorial 1)) (newline)
(display "factorial(5) = ") (display (factorial 5)) (newline)
(display "factorial(10) = ") (display (factorial 10)) (newline)

;; Using Y combinator to define fibonacci without explicit recursion
(define fibonacci
  (Y (lambda (fib)
       (lambda (n)
         (cond ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (fib (- n 1))
                        (fib (- n 2)))))))))

;; Test fibonacci
(display "\nFibonacci using Y combinator:\n")
(display "fibonacci(0) = ") (display (fibonacci 0)) (newline)
(display "fibonacci(1) = ") (display (fibonacci 1)) (newline)
(display "fibonacci(2) = ") (display (fibonacci 2)) (newline)
(display "fibonacci(3) = ") (display (fibonacci 3)) (newline)
(display "fibonacci(4) = ") (display (fibonacci 4)) (newline)
(display "fibonacci(5) = ") (display (fibonacci 5)) (newline)
(display "fibonacci(10) = ") (display (fibonacci 10)) (newline)

;; Explanation of how Y combinator creates a strange loop
(display "\nExplanation of Y combinator as a strange loop:\n")
(display "The Y combinator creates a strange loop through self-application:\n")
(display "1. It takes a functional F that expects a function as input\n")
(display "2. It creates a fixed point of F - a function f where f = F(f)\n")
(display "3. This is achieved through the self-application (x x)\n")
(display "4. This creates a 'loop' where the function effectively calls itself\n")
(display "   without needing explicit recursion in its definition\n")
