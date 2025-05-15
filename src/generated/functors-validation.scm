;; First, define some simple predicates
(define (is-positive? n) (> n 0))
(define (is-numeric? str) 
  (string->number str))
(define (is-length? n str)
  (= (string-length str) n))

;; A functor that lifts a predicate to work on lists
(define (list-validator-functor pred)
  (lambda (lst)
    (if (null? lst)
        #t
        (and (pred (car lst))
             ((list-validator-functor pred) (cdr lst))))))

;; A functor that combines predicates
(define (and-functor pred1 pred2)
  (lambda (x)
    (and (pred1 x) (pred2 x))))

;; Example: ZipCode validator as a composition of functors
(define (is-zipcode? str)
  (and (is-length? 5 str)
       (is-numeric? str)))

;; Test our validators
(display "Testing validation functors:\n")
(display "Is 12345 a valid zipcode? ")
(display (is-zipcode? "12345"))
(newline)

(display "Is 'abc' a valid zipcode? ")
(display (is-zipcode? "abc"))
(newline)

(display "Are all numbers in [1, 2, 3] positive? ")
(display ((list-validator-functor is-positive?) '(1 2 3)))
(newline)

(display "Are all numbers in [1, -2, 3] positive? ")
(display ((list-validator-functor is-positive?) '(1 -2 3)))
(newline)
