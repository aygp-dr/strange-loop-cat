;; Email and Phone Validation Library using Functors

;; Basic predicates (arrows in the category of predicates)
(define (is-length? n str)
  (= (string-length str) n))

(define (matches-pattern? pattern str)
  (and (string? str)
       (regexp-exec pattern str)))

(define (all-of pred-list)
  (lambda (x)
    (every (lambda (pred) (pred x)) pred-list)))

(define (any-of pred-list)
  (lambda (x)
    (any (lambda (pred) (pred x)) pred-list)))

;; Email validation
(define email-pattern
  (make-regexp "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))

(define (is-email? str)
  (matches-pattern? email-pattern str))

;; Phone validation for multiple formats
(define us-phone-pattern
  (make-regexp "^\\(?([0-9]{3})\\)?[-. ]?([0-9]{3})[-. ]?([0-9]{4})$"))

(define intl-phone-pattern
  (make-regexp "^\\+[0-9]{1,3}[-. ]?([0-9]{1,3})?[-. ]?([0-9]{3})[-. ]?([0-9]{4})$"))

(define (is-us-phone? str)
  (matches-pattern? us-phone-pattern str))

(define (is-intl-phone? str)
  (matches-pattern? intl-phone-pattern str))

(define (is-phone? str)
  ((any-of (list is-us-phone? is-intl-phone?)) str))

;; Functor that lifts a validator to work on maybe/option types
(define (option-validator-functor validator)
  (lambda (maybe-val)
    (cond ((not maybe-val) #f)             ;; Null case
          ((equal? maybe-val 'undefined) #f) ;; Undefined case
          (else (validator maybe-val)))))   ;; Regular case

;; Functor that lifts a validator to work on lists
(define (list-validator-functor validator)
  (lambda (lst)
    (and (list? lst)
         (every validator lst))))

;; Create validators for collections
(define is-email-list? (list-validator-functor is-email?))
(define is-phone-list? (list-validator-functor is-phone?))
(define is-optional-email? (option-validator-functor is-email?))
(define is-optional-phone? (option-validator-functor is-phone?))

;; Test the validation library
(display "EMAIL VALIDATION:\n")
(display "Valid email (test@example.com): ")
(display (is-email? "test@example.com"))
(newline)

(display "Invalid email (test@): ")
(display (is-email? "test@"))
(newline)

(display "List of valid emails: ")
(display (is-email-list? '("a@b.com" "test@example.com")))
(newline)

(display "List with invalid email: ")
(display (is-email-list? '("a@b.com" "invalid")))
(newline)

(display "Optional email (valid): ")
(display (is-optional-email? "contact@example.org"))
(newline)

(display "Optional email (null): ")
(display (is-optional-email? #f))
(newline)

(display "\nPHONE VALIDATION:\n")
(display "Valid US phone (555-123-4567): ")
(display (is-phone? "555-123-4567"))
(newline)

(display "Valid international phone (+1 555-123-4567): ")
(display (is-phone? "+1 555-123-4567"))
(newline)

(display "Invalid phone (abc-def-ghij): ")
(display (is-phone? "abc-def-ghij"))
(newline)

(display "List of valid phones: ")
(display (is-phone-list? '("555-123-4567" "+1 555-123-4567")))
(newline)

(display "List with invalid phone: ")
(display (is-phone-list? '("555-123-4567" "invalid")))
(newline)
