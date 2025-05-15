;; List as a recursive type
(define-record-type <list-f>
  (make-list-f head tail)
  list-f?
  (head list-f-head)
  (tail list-f-tail))

;; Fixed point wrapper
(define-record-type <fix>
  (make-fix unfix)
  fix?
  (unfix fix-unfix))

;; Constructors for lists
(define empty-list
  (make-fix (make-list-f #f #f)))

(define (cons-list head tail)
  (make-fix (make-list-f head tail)))

;; Tree functor
(define-record-type <tree-f>
  (make-tree-f value left right)
  tree-f?
  (value tree-f-value)
  (left tree-f-left)
  (right tree-f-right))

;; Constructors for trees
(define (leaf value)
  (make-fix (make-tree-f value #f #f)))

(define (node value left right)
  (make-fix (make-tree-f value left right)))

;; Stream as a recursive type
(define-record-type <stream>
  (make-stream head tail-thunk)
  stream?
  (head stream-head)
  (tail-thunk stream-tail-thunk))

;; Stream operations
(define (stream-tail stream)
  ((stream-tail-thunk stream)))

(define (stream-map f stream)
  (make-stream 
   (f (stream-head stream))
   (lambda () (stream-map f (stream-tail stream)))))

;; Catamorphism for lists
(define (list-cata algebra lst)
  (if (null? lst)
      (algebra 'nil '())
      (algebra 'cons (car lst) 
               (list-cata algebra (cdr lst)))))

;; Example: sum using catamorphism
(define (sum lst)
  (list-cata 
   (lambda (tag . args)
     (case tag
       ((nil) 0)
       ((cons) (+ (car args) (cadr args)))))
   lst))

;; Anamorphism for lists
(define (list-ana coalgebra seed)
  (let ((result (coalgebra seed)))
    (if (eq? (car result) 'nil)
        '()
        (cons (cadr result)
              (list-ana coalgebra (caddr result))))))

;; Example: range using anamorphism
(define (range start end)
  (list-ana
   (lambda (n)
     (if (> n end)
         (list 'nil)
         (list 'cons n (+ n 1))))
   start))

;; Hylomorphism (composition of ana and cata)
(define (list-hylo algebra coalgebra seed)
  (list-cata algebra (list-ana coalgebra seed)))

;; Example: factorial using hylomorphism
(define (factorial n)
  (list-hylo
   (lambda (tag . args)  ; Algebra: multiply the elements
     (case tag
       ((nil) 1)
       ((cons) (* (car args) (cadr args)))))
   (lambda (n)  ; Coalgebra: generate factors
     (if (<= n 1)
         (list 'nil)
         (list 'cons n (- n 1))))
   n))

;; Paramorphism for lists
(define (list-para algebra lst)
  (if (null? lst)
      (algebra 'nil '() '())
      (algebra 'cons 
              (car lst) 
              (cdr lst)
              (list-para algebra (cdr lst)))))

;; Example: fibonacci using paramorphism
(define (fibonacci n)
  (list-para
   (lambda (tag . args)
     (case tag
       ((nil) 0)
       ((cons) (if (= (car args) 1)
                   1
                   (+ (caddr args) (if (null? (cadr args))
                                      0
                                      (caaddr args)))))))
   (range 1 n)))

;; Y combinator in Scheme
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;; Example: factorial using Y combinator
(define factorial
  (Y (lambda (fact)
       (lambda (n)
         (if (zero? n)
             1
             (* n (fact (- n 1))))))))

;; Z combinator for call-by-value languages
(define Z
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

;; Mutual recursion with fixed point combinators
(define even-odd
  ((lambda (h)
     (let ((even? (car h))
           (odd? (cdr h)))
       (cons even? odd?)))
   (Z (lambda (h)
        (let ((even? (lambda (n)
                       (if (zero? n)
                           #t
                           ((cdr h) (- n 1)))))
              (odd? (lambda (n)
                      (if (zero? n)
                          #f
                          ((car h) (- n 1))))))
          (cons even? odd?))))))

;; Finding fixed points of numeric functions
(define (fixed-point f guess tolerance)
  (let ((next (f guess)))
    (if (< (abs (- next guess)) tolerance)
        next
        (fixed-point f next tolerance))))

;; Example: square root via fixed point
(define (sqrt x)
  (fixed-point 
   (lambda (y) (/ (+ y (/ x y)) 2))
   1.0
   0.00001))

;; MIU system rules as endofunctors
(define (miu-rule1 str)
  (if (string-suffix? "I" str)
      (string-append str "U")
      str))

(define (miu-rule2 str)
  (if (string-prefix? "M" str)
      (string-append str (substring str 1))
      str))

(define (miu-rule3 str)
  (let ((pattern "III"))
    (let loop ((i 0))
      (if (> (+ i 3) (string-length str))
          str
          (if (string=? (substring str i (+ i 3)) pattern)
              (string-append 
               (substring str 0 i)
               "U"
               (substring str (+ i 3)))
              (loop (+ i 1)))))))

(define (miu-rule4 str)
  (let ((pattern "UU"))
    (let loop ((i 0))
      (if (> (+ i 2) (string-length str))
          str
          (if (string=? (substring str i (+ i 2)) pattern)
              (string-append 
               (substring str 0 i)
               (substring str (+ i 2)))
              (loop (+ i 1)))))))

;; TNT system as recursive data type
(define-record-type <tnt-term>
  (make-tnt-term type args)
  tnt-term?
  (type tnt-term-type)
  (args tnt-term-args))

;; TNT constructors
(define (tnt-var name)
  (make-tnt-term 'var (list name)))

(define (tnt-plus a b)
  (make-tnt-term 'plus (list a b)))

(define (tnt-times a b)
  (make-tnt-term 'times (list a b)))

(define (tnt-equals a b)
  (make-tnt-term 'equals (list a b)))

(define (tnt-forall var formula)
  (make-tnt-term 'forall (list var formula)))

;; A simple quine in Scheme (self-reproducing program)
(define quine
  '((lambda (x) 
      (list x (list 'quote x)))
    '(lambda (x) 
       (list x (list 'quote x)))))

;; Evaluate to demonstrate
(equal? quine (eval quine))  ;; Should be #t

;; Fixed points in clock arithmetic
(define (clock-plus-n modulus n)
  (lambda (hour)
    (modulo (+ hour n) modulus)))

(define (find-fixed-points f domain)
  (filter (lambda (x) (= x (f x))) domain))

;; Example: finding fixed points of +12 in 24-hour clock
(define hours-24 (iota 24))
(define plus-12-mod-24 (clock-plus-n 24 12))
(find-fixed-points plus-12-mod-24 hours-24)  ;; Should be empty

;; Example: finding fixed points of +12 in 12-hour clock
(define hours-12 (iota 12))
(define plus-12-mod-12 (clock-plus-n 12 12))
(find-fixed-points plus-12-mod-12 hours-12)  ;; Should be all hours
