  ;; Define a variable
  (define x 42)
  
  ;; Define a function
  (define (square x)
    (* x x))
  
  ;; Lambda expression
  (define cube
    (lambda (x)
      (* x x x)))
  
  ;; Conditionals
  (define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))
  
  ;; Let expressions
  (define (hypotenuse a b)
    (let ((a-squared (square a))
          (b-squared (square b)))
      (sqrt (+ a-squared b-squared))))

  ;; List operations
  (define numbers '(1 2 3 4 5))
  
  (define (sum lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst)))))
  
  ;; Higher-order functions
  (define (map f lst)
    (if (null? lst)
        '()
        (cons (f (car lst))
              (map f (cdr lst)))))
  
  (define squares
    (map square numbers))
  
  ;; List comprehension with filter
  (define (filter pred lst)
    (cond ((null? lst) '())
          ((pred (car lst))
           (cons (car lst) (filter pred (cdr lst))))
          (else (filter pred (cdr lst)))))
  
  (define evens
    (filter even? numbers))

  ;; Standard recursion
  (define (length lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst)))))
  
  ;; Tail recursive version
  (define (length-tail lst)
    (define (iter lst acc)
      (if (null? lst)
          acc
          (iter (cdr lst) (+ acc 1))))
    (iter lst 0))
  
  ;; Mutual recursion
  (define (even? n)
    (if (= n 0)
        #t
        (odd? (- n 1))))
  
  (define (odd? n)
    (if (= n 0)
        #f
        (even? (- n 1))))

  ;; Simple non-local exit
  (define (find-first pred lst)
    (call/cc
     (lambda (return)
       (for-each (lambda (x)
                   (if (pred x)
                       (return x)))
                 lst)
       #f)))
  
  ;; Implementing generators
  (define (make-generator lst)
    (let ((next-continuation #f))
      (define (next)
        (call/cc
         (lambda (cc)
           (set! next-continuation cc)
           (if next-continuation
               (next-continuation #f)
               'done))))
      
      (define (generator)
        (call/cc
         (lambda (cc)
           (set! next-continuation cc)
           (let loop ((remaining lst))
             (if (null? remaining)
                 (begin
                   (set! next-continuation #f)
                   'done)
                 (begin
                   (call/cc
                    (lambda (cc)
                      (set! next-continuation cc)
                      (generator-return (car remaining))))
                   (loop (cdr remaining))))))))
      generator))

  ;; Simple macro with syntax-rules
  (define-syntax when
    (syntax-rules ()
      ((when condition body ...)
       (if condition
           (begin body ...)))))
  
  ;; More complex macro
  (define-syntax for
    (syntax-rules ()
      ((for (var init pred step) body ...)
       (let loop ((var init))
         (when pred
           body ...
           (loop step))))))
  
  ;; Usage example
  (for (i 0 (< i 10) (+ i 1))
       (display i)
       (newline))

  ;; Returning multiple values
  (define (divmod x y)
    (values (quotient x y)
            (remainder x y)))
  
  ;; Consuming multiple values
  (define (show-divmod x y)
    (call-with-values
        (lambda () (divmod x y))
      (lambda (q r)
        (format #t "~a divided by ~a is ~a remainder ~a\n"
                x y q r))))
  
  (show-divmod 17 5)

  ;; Parameters for dynamic binding
  (define current-output-port
    (make-parameter (current-output-port)))
  
  ;; Using parameters
  (define (with-output-to-string thunk)
    (let ((port (open-output-string)))
      (parameterize ((current-output-port port))
        (thunk)
        (get-output-string port))))
  
  (define result
    (with-output-to-string
     (lambda ()
       (display "Hello")
       (newline)
       (display "World"))))

  ;; Defining a module
  (define-module (myproject utils)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1)
    #:export (sum average))
  
  (define (sum lst)
    (fold + 0 lst))
  
  (define (average lst)
    (/ (sum lst) (length lst)))
  
  ;; Using the module
  (use-modules (myproject utils))
  
  (display (average '(1 2 3 4 5)))
  (newline)

  ;; Using GOOPS
  (use-modules (oop goops))
  
  ;; Define a class
  (define-class <person> ()
    (name #:init-keyword #:name #:accessor person-name)
    (age #:init-keyword #:age #:accessor person-age))
  
  ;; Define a method
  (define-method (greet (p <person>))
    (format #t "Hello, my name is ~a and I am ~a years old.\n"
            (person-name p)
            (person-age p)))
  
  ;; Create an instance
  (define alice (make <person> #:name "Alice" #:age 30))
  
  ;; Call the method
  (greet alice)

  ;; Define a record type
  (define-record-type <point>
    (make-point x y)
    point?
    (x point-x point-x-set!)
    (y point-y point-y-set!))
  
  ;; Methods for points
  (define (point-distance p1 p2)
    (sqrt (+ (square (- (point-x p2) (point-x p1)))
             (square (- (point-y p2) (point-y p1))))))
  
  ;; Create instances
  (define origin (make-point 0 0))
  (define p (make-point 3 4))
  
  ;; Use them
  (display (point-distance origin p))
  (newline)

  ;; Using C functions from Scheme
  (use-modules (system foreign))
  
  ;; Load a shared library
  (define libc (dynamic-link "libc"))
  
  ;; Get a function
  (define strlen
    (pointer->procedure int
                        (dynamic-func "strlen" libc)
                        (list '*)))
  
  ;; Use the function
  (define (string-length-c str)
    (strlen (string->pointer str)))
  
  (display (string-length-c "hello"))
  (newline)

  ;; Maybe functor
  (define-record-type <maybe>
    (make-maybe value)
    maybe?
    (value maybe-value))
  
  (define nothing 'nothing)
  
  (define (just value)
    (make-maybe value))
  
  (define (maybe-map f m)
    (if (eq? m nothing)
        nothing
        (just (f (maybe-value m)))))
  
  ;; List functor
  (define (list-map f lst)
    (map f lst))
  
  ;; Functor laws
  (define (test-functor-laws functor-map value)
    (let ((id (lambda (x) x))
          (f (lambda (x) (+ x 1)))
          (g (lambda (x) (* x 2))))
      ;; Identity law: map id = id
      (equal? (functor-map id value) value)
      ;; Composition law: map (g . f) = (map g) . (map f)
      (equal? (functor-map (lambda (x) (g (f x))) value)
              (functor-map g (functor-map f value)))))

  ;; Maybe monad
  (define (maybe-return x)
    (just x))
  
  (define (maybe-bind m f)
    (if (eq? m nothing)
        nothing
        (f (maybe-value m))))
  
  ;; List monad
  (define (list-return x)
    (list x))
  
  (define (list-bind lst f)
    (apply append (map f lst)))
  
  ;; State monad
  (define (make-state-monad state-type)
    (let ()
      (define (return x)
        (lambda (s) (cons x s)))
      
      (define (bind m f)
        (lambda (s)
          (let* ((result (m s))
                 (value (car result))
                 (new-state (cdr result)))
            ((f value) new-state))))
      
      (define (get)
        (lambda (s) (cons s s)))
      
      (define (put new-state)
        (lambda (s) (cons (if #f #f) new-state)))
      
      (define (run-state m initial-state)
        (m initial-state))
      
      (define (evaluate m initial-state)
        (car (run-state m initial-state)))
      
      (define (execute m initial-state)
        (cdr (run-state m initial-state)))
      
      (list return bind get put run-state evaluate execute)))
  
  ;; Usage example
  (define state (make-state-monad '<state>))
  (define state-return (car state))
  (define state-bind (cadr state))
  (define state-get (caddr state))
  (define state-put (cadddr state))
  (define run-state (car (cddddr state)))
  
  (define counter
    (let* ((get-and-increment
            (state-bind state-get
                       (lambda (n)
                         (state-bind (state-put (+ n 1))
                                    (lambda (_)
                                      (state-return n))))))
           (increment-and-get
            (state-bind state-get
                       (lambda (n)
                         (let ((n1 (+ n 1)))
                           (state-bind (state-put n1)
                                      (lambda (_)
                                        (state-return n1))))))))
      (state-bind get-and-increment
                 (lambda (n1)
                   (state-bind increment-and-get
                              (lambda (n2)
                                (state-return (cons n1 n2))))))))
  
  (display (run-state counter 0))
  (newline)

  ;; Continuation monad
  (define (cont-return x)
    (lambda (k) (k x)))
  
  (define (cont-bind m f)
    (lambda (k)
      (m (lambda (v)
           ((f v) k)))))
  
  ;; call/cc in terms of the continuation monad
  (define (call/cc-monad f)
    (lambda (k)
      ((f (lambda (v)
            (lambda (_) (k v))))
       k)))
  
  ;; Example usage
  (define (test-continuation)
    ((cont-bind
      (cont-return 42)
      (lambda (x)
        (cont-bind
         (call/cc-monad
          (lambda (k)
            (if (even? x)
                (cont-return (+ x 1))
                (k x))))
         (lambda (y)
           (cont-return (* y 2))))))
     (lambda (result) (display result) (newline))))

  ;; Simple metacircular evaluator
  (define (evaluate exp env)
    (cond
     ((self-evaluating? exp) exp)
     ((variable? exp) (lookup-variable exp env))
     ((quoted? exp) (text-of-quotation exp))
     ((assignment? exp) (eval-assignment exp env))
     ((definition? exp) (eval-definition exp env))
     ((if? exp) (eval-if exp env))
     ((lambda? exp) (make-procedure (lambda-parameters exp)
                                   (lambda-body exp)
                                   env))
     ((begin? exp) (eval-sequence (begin-actions exp) env))
     ((application? exp) (apply-procedure (evaluate (operator exp) env)
                                         (list-of-values (operands exp) env)))
     (else (error "Unknown expression type" exp))))
  
  ;; Recursive evaluation of lists
  (define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (evaluate (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))
  
  ;; Conditional evaluation
  (define (eval-if exp env)
    (if (true? (evaluate (if-predicate exp) env))
        (evaluate (if-consequent exp) env)
        (evaluate (if-alternative exp) env)))

  ;; Simplest quine
  ((lambda (x) (list x (list 'quote x)))
   '(lambda (x) (list x (list 'quote x))))
  
  ;; Another quine
  (define quine
    '((lambda (s)
        (display
         (list (car s)
               (list 'quote (cadr s)))))
      '((lambda (s)
          (display
           (list (car s)
                 (list 'quote (cadr s)))))
        'dummy)))

  ;; Y combinator
  (define Y
    (lambda (f)
      ((lambda (x) (f (lambda (y) ((x x) y))))
       (lambda (x) (f (lambda (y) ((x x) y)))))))
  
  ;; Factorial using Y combinator
  (define factorial
    (Y (lambda (fact)
         (lambda (n)
           (if (zero? n)
               1
               (* n (fact (- n 1))))))))
  
  (display (factorial 5))
  (newline)

  ;; Simple reflective interpreter
  (define (make-evaluator)
    (lambda (exp env)
      (cond
       ((self-evaluating? exp) exp)
       ((variable? exp) (lookup-variable exp env))
       ((quoted? exp) (text-of-quotation exp))
       ((application? exp)
        (apply
         (evaluate (operator exp) env)
         (map (lambda (operand)
                (evaluate operand env))
              (operands exp))))
       ((lambda? exp)
        (make-procedure
         (lambda-parameters exp)
         (lambda-body exp)
         env))
       ((if? exp)
        (if (evaluate (if-predicate exp) env)
            (evaluate (if-consequent exp) env)
            (evaluate (if-alternative exp) env)))
       ((eval? exp)
        ((make-evaluator) (eval-expression exp) env))
       (else (error "Unknown expression type" exp)))))
  
  ;; Using the evaluator
  (define evaluate (make-evaluator))
  
  ;; Example of evaluating an expression
  (define result
    (evaluate
     '((lambda (x) (+ x 1)) 41)
     (make-global-environment)))
