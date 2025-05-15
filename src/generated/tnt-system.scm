;; TNT symbols and operations
(define-record-type <tnt-term>
  (make-tnt-term type value args)
  tnt-term?
  (type tnt-term-type)
  (value tnt-term-value)
  (args tnt-term-args))

;; TNT Term constructors
(define (tnt-variable name) 
  (make-tnt-term 'variable name '()))

(define (tnt-constant n) 
  (make-tnt-term 'constant n '()))

(define (tnt-successor term)
  (make-tnt-term 'successor 'S (list term)))

(define (tnt-plus a b)
  (make-tnt-term 'plus '+ (list a b)))

(define (tnt-times a b)
  (make-tnt-term 'times '* (list a b)))

;; TNT Formula constructors
(define (tnt-equals a b)
  (make-tnt-term 'equals '= (list a b)))

(define (tnt-not p)
  (make-tnt-term 'not '~ (list p)))

(define (tnt-and p q)
  (make-tnt-term 'and '^ (list p q)))

(define (tnt-or p q)
  (make-tnt-term 'or 'v (list p q)))

(define (tnt-implies p q)
  (make-tnt-term 'implies '⊃ (list p q)))

(define (tnt-forall var p)
  (make-tnt-term 'forall '∀ (list var p)))

(define (tnt-exists var p)
  (make-tnt-term 'exists '∃ (list var p)))

;; String representation of TNT terms
(define (tnt->string term)
  (case (tnt-term-type term)
    ((variable) (tnt-term-value term))
    ((constant) (number->string (tnt-term-value term)))
    ((successor) (string-append "S" (tnt->string (car (tnt-term-args term)))))
    ((plus) (string-append "(" 
                          (tnt->string (car (tnt-term-args term)))
                          "+"
                          (tnt->string (cadr (tnt-term-args term)))
                          ")"))
    ((times) (string-append "(" 
                           (tnt->string (car (tnt-term-args term)))
                           "·"
                           (tnt->string (cadr (tnt-term-args term)))
                           ")"))
    ((equals) (string-append (tnt->string (car (tnt-term-args term)))
                            "="
                            (tnt->string (cadr (tnt-term-args term)))))
    ((not) (string-append "~" (tnt->string (car (tnt-term-args term)))))
    ((and) (string-append "(" 
                         (tnt->string (car (tnt-term-args term)))
                         "^"
                         (tnt->string (cadr (tnt-term-args term)))
                         ")"))
    ((or) (string-append "(" 
                        (tnt->string (car (tnt-term-args term)))
                        "v"
                        (tnt->string (cadr (tnt-term-args term)))
                        ")"))
    ((implies) (string-append "(" 
                             (tnt->string (car (tnt-term-args term)))
                             "⊃"
                             (tnt->string (cadr (tnt-term-args term)))
                             ")"))
    ((forall) (string-append "∀" 
                            (tnt->string (car (tnt-term-args term)))
                            ":" 
                            (tnt->string (cadr (tnt-term-args term)))))
    ((exists) (string-append "∃" 
                            (tnt->string (car (tnt-term-args term)))
                            ":" 
                            (tnt->string (cadr (tnt-term-args term)))))))

;; Example: Creating and displaying a TNT formula
(define zero (tnt-constant 0))
(define a (tnt-variable "a"))
(define statement 
  (tnt-forall a (tnt-not (tnt-equals (tnt-plus a zero) zero))))

(format #t "TNT statement: ~a\n" (tnt->string statement))
