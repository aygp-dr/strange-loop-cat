;; A Scheme quine - the quintessential self-referential program
(define quine
  '((lambda (x) 
      (let ((d '(lambda (x) 
                  (let ((d '()))
                    (cons d 
                          (list 
                           (list 'quote d)))))))
        (cons d 
              (list 
               (list 'quote d))))) 
    '(lambda (x) 
       (let ((d '()))
         (cons d 
               (list 
                (list 'quote d)))))))

;; Evaluate and display the quine
(format #t "Quine result:\n~a\n" (eval quine (interaction-environment)))

;; Verify it's a true quine by comparing to original
(format #t "Is it a true quine? ~a\n" 
        (equal? quine (eval quine (interaction-environment))))
