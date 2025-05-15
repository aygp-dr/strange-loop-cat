;; Implementing the Continuation Monad to explore recursion and self-reference

;; Continuation monad constructor (unit/return)
(define (cont-return x)
  (lambda (k) (k x)))

;; Continuation monad bind
(define (cont-bind m f)
  (lambda (k)
    (m (lambda (v) ((f v) k)))))

;; Run a continuation computation with an initial continuation
(define (run-cont m)
  (m (lambda (x) x)))

;; Helper to call/cc in our continuation monad
(define (call-cc/m f)
  (lambda (k)
    (let ((cc (lambda (x)
                (lambda (_) (k x)))))
      ((f cc) k))))

;; A recursive factorial function using the continuation monad
(define (factorial-cont n)
  (if (<= n 1)
      (cont-return 1)
      (cont-bind (factorial-cont (- n 1))
                (lambda (result)
                  (cont-return (* n result))))))

;; Another example: fixed point calculation using continuations
(define (fixed-point-cont f start)
  (define tolerance 0.00001)
  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  
  (define (iterate guess)
    (cont-bind (cont-return (f guess))
               (lambda (next)
                 (if (close-enough? guess next)
                     (cont-return next)
                     (iterate next)))))
  
  (iterate start))

;; Test the factorial function
(display "Factorial using continuation monad:\n")
(display "5! = ")
(display (run-cont (factorial-cont 5)))
(newline)

;; Use cont monad for fixed point calculation
(display "\nFixed point of cos(x) using continuation monad:\n")
(display (run-cont (fixed-point-cont cos 1.0)))
(newline)

;; Demonstrate call/cc with a simple example
(define (example-with-cc)
  (cont-bind (call-cc/m 
              (lambda (escape)
                (cont-bind (cont-return 1)
                          (lambda (x)
                            (cont-bind (cont-return 2)
                                      (lambda (y)
                                        (cont-bind ((escape 100))
                                                  (lambda (z)
                                                    (cont-return 
                                                     (+ x y z))))))))))
             (lambda (result)
               (cont-return result))))

(display "\nDemonstrating call/cc with the continuation monad:\n")
(display "Result with escape: ")
(display (run-cont (example-with-cc)))
(newline)

;; Explain the connection to strange loops
(display "\nConnection to Strange Loops:\n")
(display "The continuation monad captures 'the rest of the computation' as a first-class\n")
(display "value that can be manipulated. This creates potential for strange loops when\n")
(display "we use call/cc to capture a continuation and then invoke it multiple times or\n")
(display "in unexpected contexts, creating a cyclical flow of control where 'the future'\n")
(display "can influence 'the past'.\n")
