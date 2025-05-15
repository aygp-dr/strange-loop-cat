;; MIU formal system from GEB
(define (miu-rule1 str)
  (if (string=? (string-take-right str 1) "I")
      (string-append str "U")
      str))

(define (miu-rule2 str)
  (if (string=? (string-take str 1) "M")
      (string-append "M" (string-append str (substring str 1)))
      str))

(define (miu-rule3 str)
  (let* ((pattern "III")
         (idx (string-contains str pattern)))
    (if idx
        (string-append (substring str 0 idx) 
                       "U" 
                       (substring str (+ idx (string-length pattern))))
        str)))

(define (miu-rule4 str)
  (let* ((pattern "UU")
         (idx (string-contains str pattern)))
    (if idx
        (string-append (substring str 0 idx) 
                       (substring str (+ idx (string-length pattern))))
        str)))

;; Check if a string is a valid MIU string
(define (valid-miu? str)
  (and (> (string-length str) 0)
       (string=? (string-take str 1) "M")
       (string-every (lambda (c) (member c '(#\M #\I #\U))) str)))

;; Generate all possible MIU strings up to a certain level
(define (generate-miu-strings start max-depth)
  (define seen (make-hash-table))
  (define (explore str depth)
    (if (or (>= depth max-depth) (hash-ref seen str))
        '()
        (begin
          (hash-set! seen str #t)
          (cons str
                (append (explore (miu-rule1 str) (+ depth 1))
                        (explore (miu-rule2 str) (+ depth 1))
                        (explore (miu-rule3 str) (+ depth 1))
                        (explore (miu-rule4 str) (+ depth 1)))))))
  (explore start 0))

;; GEB-style demonstration
(define miu-strings (generate-miu-strings "MI" 4))
(format #t "MIU strings: ~a\n" miu-strings)
(format #t "Is MU derivable? ~a\n" 
        (member "MU" miu-strings))
