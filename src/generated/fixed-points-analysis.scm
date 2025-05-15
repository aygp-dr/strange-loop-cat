;; Analyze whether a function has fixed points in a given domain

(define (analyze-fixed-points f domain-min domain-max samples)
  (let* ((points (sample-domain domain-min domain-max samples))
         (values (map f points))
         (differences (map - values points))
         (sign-changes
          (count (lambda (pair)
                   (let ((a (car pair))
                         (b (cadr pair)))
                     (not (= (if (>= a 0) 1 -1)
                             (if (>= b 0) 1 -1)))))
                 (zip differences (cdr differences)))))
    
    ;; Intermediate value theorem: if the difference changes sign,
    ;; there must be a fixed point in between
    (if (> sign-changes 0)
        (format #t "Function likely has ~a fixed points in [~a, ~a]\n" 
                sign-changes domain-min domain-max)
        (format #t "No evidence of fixed points in [~a, ~a]\n" 
                domain-min domain-max))))

;; Examples:
(format #t "Analyzing f(x) = √x:\n")
(analyze-fixed-points sqrt 0.1 2.0 20)

(format #t "\nAnalyzing f(x) = 2x:\n")
(analyze-fixed-points (lambda (x) (* 2 x)) -5.0 5.0 20)

(format #t "\nAnalyzing f(x) = cos(x):\n")
(analyze-fixed-points cos 0.0 1.0 20)

(format #t "\nAnalyzing f(x) = sin(x):\n")
(analyze-fixed-points sin -2.0 2.0 40)
