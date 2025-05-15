;; Hofstadter's H-sequence: H(n) = n - H(H(H(n-1)))
(define h-memo (make-hash-table))

(define (hofstadter-h n)
  (if (= n 0)
      0
      (or (hash-ref h-memo n)
          (let ((result (- n (hofstadter-h (hofstadter-h (hofstadter-h (- n 1)))))))
            (hash-set! h-memo n result)
            result))))

;; Generate sequence terms
(define (h-sequence n)
  (map hofstadter-h (iota n)))

(format #t "First 20 terms of Hofstadter's H-sequence:\n~a\n" 
        (h-sequence 20))
