;; GEB Levels of Meaning with Type Theory
(define-record-type <geb-level>
  (make-geb-level name description elements operations)
  geb-level?
  (name level-name)
  (description level-description)
  (elements level-elements)
  (operations level-operations))

;; Define the GEB meaning levels
(define level-0 
  (make-geb-level
   "Level 0: Strings"
   "The basic symbols and strings of formal systems"
   '("M" "I" "U" "MI" "MU" "MIU" "MIIU")
   (list
    (cons 'append string-append)
    (cons 'length string-length))))

(define level-1
  (make-geb-level
   "Level 1: Rules"
   "The rules that operate on strings"
   (list miu-rule1 miu-rule2 miu-rule3 miu-rule4)
   (list
    (cons 'apply (lambda (rule string) (rule string)))
    (cons 'compose (lambda (rule1 rule2)
                     (lambda (s) (rule2 (rule1 s))))))))

(define level-2
  (make-geb-level
   "Level 2: Metarules"
   "Rules for generating or selecting rules"
   (list 
    (lambda (rules) 
      (list-ref rules (random (length rules))))
    (lambda (rules)
      (lambda (s)
        ((fold (lambda (rule result)
                (lambda (str) (rule (result str))))
              (lambda (x) x)
              rules)
         s))))
   (list
    (cons 'select car)
    (cons 'combine cadr))))

;; Create Escher-like Strange Loop: levels that reference each other
(define (connect-strange-loop levels)
  (for-each
   (lambda (level-pair)
     (let ((lower (car level-pair))
           (higher (cadr level-pair)))
       ;; Add reference from higher to lower level
       (set! (level-elements higher)
             (cons (level-name lower) (level-elements higher)))
       ;; Add reference from lower to higher level
       (set! (level-operations lower)
             (cons (cons 'ascend 
                         (lambda (x) 
                           (format #t "Rising to ~a\n" (level-name higher))
                           (level-name higher)))
                   (level-operations lower)))))
   (zip levels (append (cdr levels) (list (car levels))))))

;; Create and connect levels
(define geb-levels (list level-0 level-1 level-2))
(connect-strange-loop geb-levels)

;; Demonstrate the strange loop
(define (traverse-geb-levels levels count)
  (let loop ((current-level (car levels))
             (steps 0))
    (format #t "At ~a: ~a\n" 
            (level-name current-level)
            (level-description current-level))
    (if (>= steps count)
        'done
        (let* ((ops (level-operations current-level))
               (ascend-op (cdr (assoc 'ascend ops)))
               (next-level-name (ascend-op 'current))
               (next-level (find (lambda (l) 
                                   (string=? (level-name l) next-level-name))
                                 levels)))
          (loop next-level (+ steps 1))))))

(traverse-geb-levels geb-levels 6)
