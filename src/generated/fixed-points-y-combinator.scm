;; Y combinator: a fixed point combinator that enables recursion
(define (Y f)
  ((lambda (x) (f (lambda (y) ((x x) y))))
   (lambda (x) (f (lambda (y) ((x x) y))))))

;; Using Y to create factorial without explicit recursion
(define factorial
  (Y (lambda (fact)
       (lambda (n)
         (if (= n 0)
             1
             (* n (fact (- n 1))))))))

;; Test the Y combinator
(format #t "Factorial of 5: ~a\n" (factorial 5))
(format #t "Factorial of 10: ~a\n" (factorial 10))

;; Y combinator is the fixed point of the higher-order function:
(define (factorial-generator f)
  (lambda (n)
    (if (= n 0)
        1
        (* n (f (- n 1))))))

;; Y(factorial-generator) = factorial-generator(Y(factorial-generator))
;; This is the fixed point equation!
