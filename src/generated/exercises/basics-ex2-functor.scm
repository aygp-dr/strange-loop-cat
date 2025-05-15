;; First include the module loading code
(add-to-load-path "../src/guile")
(use-modules (category))

;; Create the source category (a simple linear category)
(define s-a (make-object "S_A"))
(define s-b (make-object "S_B"))
(define s-mor (make-morphism "s_mor" s-a s-b))
(define s-id-a (identity s-a))
(define s-id-b (identity s-b))
(define source-cat (make-category "Source" (list s-a s-b) (list s-mor s-id-a s-id-b)))

;; Create the target category (a product category)
(define t-a (make-object "T_A"))
(define t-b (make-object "T_B"))
(define t-c (make-object "T_C"))
(define t-mor1 (make-morphism "t_mor1" t-a t-b))
(define t-mor2 (make-morphism "t_mor2" t-b t-c))
(define t-id-a (identity t-a))
(define t-id-b (identity t-b))
(define t-id-c (identity t-c))
(define target-cat (make-category "Target" 
                                 (list t-a t-b t-c) 
                                 (list t-mor1 t-mor2 t-id-a t-id-b t-id-c)))

;; Define the functor mapping objects
(define (functor-obj obj)
  (cond ((equal? obj s-a) t-a)
        ((equal? obj s-b) t-c)
        (else (error "Object not in source category"))))

;; Define the functor mapping morphisms
(define (functor-mor mor)
  (let ((name (cadr mor))
        (domain (caddr mor))
        (codomain (cadddr mor)))
    (cond ((equal? name "s_mor") 
           (compose t-mor1 t-mor2))  ;; Map s_mor to composite t_mor2 ∘ t_mor1
          ((equal? name "id_S_A") t-id-a)
          ((equal? name "id_S_B") t-id-c)
          (else (error "Morphism not in source category")))))

;; Test the functor
(display "SOURCE CATEGORY:\n")
(display source-cat) (newline)

(display "\nTARGET CATEGORY:\n")
(display target-cat) (newline)

(display "\nFUNCTOR MAPPING:\n")
(display "F(S_A) = ") (display (functor-obj s-a)) (newline)
(display "F(S_B) = ") (display (functor-obj s-b)) (newline)
(display "F(s_mor) = ") (display (functor-mor s-mor)) (newline)
(display "F(id_S_A) = ") (display (functor-mor s-id-a)) (newline)
