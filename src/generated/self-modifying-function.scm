;; Self-modifying function: A strange loop that changes its own definition
(define (make-self-modifying-function)
  (let ((counter 0)
        (implementations
         (list
          (lambda (x) (* x 2))          ;; Initial implementation
          (lambda (x) (+ x 1))          ;; First transformation
          (lambda (x) (expt x 2))       ;; Second transformation
          (lambda (x) (- x 10)))))      ;; Final transformation
    (lambda (x)
      (let ((current-impl (list-ref implementations 
                                    (modulo counter (length implementations))))
            (result '()))
        (set! result (current-impl x))
        (set! counter (+ counter 1)) ;; Evolve for next call
        (format #t "Function evolved to version ~a\n" 
                (modulo counter (length implementations)))
        result))))

;; Create and use a self-modifying function
(define strange-f (make-self-modifying-function))
(format #t "Result 1: ~a\n" (strange-f 5))  ;; Uses x*2
(format #t "Result 2: ~a\n" (strange-f 5))  ;; Uses x+1  
(format #t "Result 3: ~a\n" (strange-f 5))  ;; Uses x^2
(format #t "Result 4: ~a\n" (strange-f 5))  ;; Uses x-10
(format #t "Result 5: ~a\n" (strange-f 5))  ;; Back to x*2
