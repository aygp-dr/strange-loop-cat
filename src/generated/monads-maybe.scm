;; Load the category module
(add-to-load-path "../src/guile")
(use-modules (category))

(display "Category theory module loaded successfully.\n")

;; The Maybe monad in the category of sets
;; For an object (set) A, Maybe(A) = A ∪ {Nothing}
;; For a morphism f: A → B, Maybe(f) maps:
;;  - Each a ∈ A to f(a) ∈ B
;;  - Nothing to Nothing

;; We'll represent Maybe(A) as either:
;; - (just value) for a value from A
;; - 'nothing for the additional Nothing element

;; Unit: A → Maybe(A)
(define (unit-maybe x)
  (list 'just x))

;; Join: Maybe(Maybe(A)) → Maybe(A)
(define (join-maybe mmx)
  (if (eq? mmx 'nothing)
      'nothing
      (let ((mx (cadr mmx)))
        (if (eq? mx 'nothing)
            'nothing
            (if (and (list? mx) (eq? (car mx) 'just))
                mx
                'nothing)))))

;; Bind operation (derived from unit and join)
(define (bind-maybe mx f)
  (if (eq? mx 'nothing)
      'nothing
      (f (cadr mx))))

;; Example usage:
(display "Maybe Monad Examples:\n")

(display "\nUnit examples:\n")
(display "unit(5) = ") (display (unit-maybe 5)) (newline)
(display "unit(\"hello\") = ") (display (unit-maybe "hello")) (newline)

(display "\nJoin examples:\n")
(display "join(just(just(5))) = ") (display (join-maybe (list 'just (list 'just 5)))) (newline)
(display "join(just(nothing)) = ") (display (join-maybe (list 'just 'nothing))) (newline)
(display "join(nothing) = ") (display (join-maybe 'nothing)) (newline)

;; Define some functions for the bind examples
(define (safe-sqrt x)
  (if (>= x 0)
      (unit-maybe (sqrt x))
      'nothing))

(define (safe-reciprocal x)
  (if (not (zero? x))
      (unit-maybe (/ 1 x))
      'nothing))

(display "\nBind examples:\n")
(display "bind(just(4), safe-sqrt) = ") (display (bind-maybe (unit-maybe 4) safe-sqrt)) (newline)
(display "bind(just(-4), safe-sqrt) = ") (display (bind-maybe (unit-maybe -4) safe-sqrt)) (newline)
(display "bind(just(0), safe-reciprocal) = ") (display (bind-maybe (unit-maybe 0) safe-reciprocal)) (newline)
(display "bind(nothing, safe-sqrt) = ") (display (bind-maybe 'nothing safe-sqrt)) (newline)

;; Chain of computations with bind
(display "\nChained computation:\n")
(define result 
  (bind-maybe (unit-maybe 16) 
              (lambda (x) 
                (bind-maybe (safe-sqrt x)
                            safe-reciprocal))))
(display "16 → √16 → 1/√16 = ") (display result) (newline)

(define result-fail
  (bind-maybe (unit-maybe -16) 
              (lambda (x) 
                (bind-maybe (safe-sqrt x)
                            safe-reciprocal))))
(display "-16 → √-16 → error = ") (display result-fail) (newline)
