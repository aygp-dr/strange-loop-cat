;; Create Gödel numbering for TNT
(define (tnt-godel-encode term)
  (define symbol-codes
    '((S . 1) (0 . 3) (= . 5) (+ . 7) (· . 9)
      (~ . 11) (v . 13) (^ . 15) (⊃ . 17) (∀ . 19) (∃ . 21)
      (a . 23) (b . 25) (c . 27) (d . 29) (e . 31)))
  
  (define (encode-symbol sym)
    (let ((code (assoc sym symbol-codes)))
      (if code (cdr code) 
          (if (number? sym) (* 2 sym) 101))))
  
  (define (encode-term t)
    (case (tnt-term-type t)
      ((variable) (encode-symbol (tnt-term-value t)))
      ((constant) (encode-symbol (tnt-term-value t)))
      (else (let ((code (encode-symbol (tnt-term-value t)))
                  (args-encoded (map encode-term (tnt-term-args t))))
              (apply + (* code (expt 2 (length args-encoded)))
                     args-encoded)))))
  
  (encode-term term))

;; Create a self-referential TNT statement (Gödel-like)
(define (create-godel-statement)
  (let* ((var-a (tnt-variable "a"))
         ;; Statement pattern: "This statement is not provable"
         (self-ref (tnt-not 
                    (tnt-exists var-a
                               (tnt-equals var-a 
                                          (tnt-constant 
                                           (tnt-godel-encode 'placeholder)))))))
    ;; Calculate the actual Gödel number
    (let ((godel-num (tnt-godel-encode self-ref)))
      ;; Update the placeholder with the actual Gödel number
      (tnt-not 
       (tnt-exists var-a
                  (tnt-equals var-a 
                             (tnt-constant godel-num)))))))

;; Demonstrate the Gödel statement
(define g-statement (create-godel-statement))
(format #t "Gödel statement: ~a\n" (tnt->string g-statement))
(format #t "Gödel number: ~a\n" (tnt-godel-encode g-statement))
