;; Hofstadter's G-sequence: G(n) = n - G(G(n-1))
(define g-memo (make-hash-table))

(define (hofstadter-g n)
  (if (= n 0)
      0
      (or (hash-ref g-memo n)
          (let ((result (- n (hofstadter-g (hofstadter-g (- n 1))))))
            (hash-set! g-memo n result)
            result))))

;; Generate sequence terms
(define (g-sequence n)
  (map hofstadter-g (iota n)))

(format #t "First 20 terms of Hofstadter's G-sequence:\n~a\n" 
        (g-sequence 20))
