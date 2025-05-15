;; Map is a direct example of a functor
;; It transforms a function f: A → B into a function [A] → [B]
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (my-map f (cdr lst)))))

;; Filter is related to the "powerset functor"
(define (my-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (my-filter pred (cdr lst))))
        (else
         (my-filter pred (cdr lst)))))

;; Reduce (fold) can be viewed as a natural transformation
;; from the list functor to the identity functor
(define (my-reduce f init lst)
  (if (null? lst)
      init
      (my-reduce f 
                (f init (car lst))
                (cdr lst))))

;; Example: Using map, filter, reduce with a list of numbers
(define numbers '(1 2 3 4 5 6 7 8 9 10))

(display "Original list: ")
(display numbers)
(newline)

(display "After mapping (double): ")
(display (my-map (lambda (x) (* 2 x)) numbers))
(newline)

(display "After filtering (even numbers): ")
(display (my-filter (lambda (x) (= (remainder x 2) 0)) numbers))
(newline)

(display "After reducing (sum): ")
(display (my-reduce + 0 numbers))
(newline)
