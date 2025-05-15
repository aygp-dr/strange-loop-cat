;; Hofstadter's Q-sequence: Q(n) = Q(n-Q(n-1)) + Q(n-Q(n-2)) for n>2
(define (hofstadter-q n)
  (if (<= n 2)
      1
      (+ (hofstadter-q (- n (hofstadter-q (- n 1))))
         (hofstadter-q (- n (hofstadter-q (- n 2)))))))

;; Optimize with memoization for practical computation
(define hofstadter-q-memo
  (let ((memo (make-hash-table)))
    (lambda (n)
      (or (hash-ref memo n)
          (let ((result
                 (if (<= n 2)
                     1
                     (+ (hofstadter-q-memo (- n (hofstadter-q-memo (- n 1))))
                        (hofstadter-q-memo (- n (hofstadter-q-memo (- n 2)))))))
            (hash-set! memo n result)
            result)))))

;; Generate sequence terms
(define (q-sequence n)
  (map hofstadter-q-memo (iota n 1)))

(format #t "First 20 terms of Hofstadter's Q-sequence:\n~a\n" 
        (q-sequence 20))
