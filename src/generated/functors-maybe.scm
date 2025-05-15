;; Define the Maybe type using tagged lists
(define (just value) (list 'just value))
(define (nothing) (list 'nothing))

(define (maybe? obj)
  (and (pair? obj)
       (or (eq? (car obj) 'just)
           (eq? (car obj) 'nothing))))

(define (just? obj)
  (and (maybe? obj) (eq? (car obj) 'just)))

(define (nothing? obj)
  (and (maybe? obj) (eq? (car obj) 'nothing)))

(define (maybe-value obj)
  (if (just? obj)
      (cadr obj)
      (error "Cannot extract value from Nothing")))

;; The Maybe functor maps a function a → b to Maybe a → Maybe b
(define (maybe-map f maybe-obj)
  (if (just? maybe-obj)
      (just (f (maybe-value maybe-obj)))
      maybe-obj))  ;; Preserve Nothing

;; Safe division example using Maybe
(define (safe-div a b)
  (if (= b 0)
      (nothing)
      (just (/ a b))))

;; Test our Maybe functor
(display "Testing the Maybe functor:\n")

(display "safe-div 10 2: ")
(display (safe-div 10 2))
(newline)

(display "safe-div 10 0: ")
(display (safe-div 10 0))
(newline)

(display "maybe-map square (just 4): ")
(display (maybe-map (lambda (x) (* x x)) (just 4)))
(newline)

(display "maybe-map square (nothing): ")
(display (maybe-map (lambda (x) (* x x)) (nothing)))
(newline)

;; Chaining operations with Maybe
(define (div-and-square a b)
  (maybe-map (lambda (x) (* x x)) (safe-div a b)))

(display "div-and-square 10 2: ")
(display (div-and-square 10 2))
(newline)

(display "div-and-square 10 0: ")
(display (div-and-square 10 0))
(newline)
