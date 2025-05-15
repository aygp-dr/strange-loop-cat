;; Clock arithmetic operations
(define (clock-plus-n clock-hours n)
  (modulo (+ clock-hours n) clock-hours))

;; Check for fixed points
(define (fixed-points-of-plus-n clock-hours n)
  (filter (lambda (hour)
            (= hour (clock-plus-n clock-hours hour n)))
          (iota clock-hours)))

;; Explore 12-hour clock
(format #t "In a 12-hour clock:\n")
(format #t "  Fixed points of +12 hours: ~a\n" 
        (fixed-points-of-plus-n 12 12))
(format #t "  Fixed points of +6 hours: ~a\n" 
        (fixed-points-of-plus-n 12 6))
(format #t "  Fixed points of +4 hours: ~a\n" 
        (fixed-points-of-plus-n 12 4))
(format #t "  Fixed points of +1 hour: ~a\n" 
        (fixed-points-of-plus-n 12 1))

;; Explore 24-hour clock
(format #t "\nIn a 24-hour clock:\n")
(format #t "  Fixed points of +12 hours: ~a\n" 
        (fixed-points-of-plus-n 24 12))
(format #t "  Fixed points of +6 hours: ~a\n" 
        (fixed-points-of-plus-n 24 6))
(format #t "  Fixed points of +8 hours: ~a\n" 
        (fixed-points-of-plus-n 24 8))

;; General pattern
(define (explain-fixed-point-pattern n m)
  (let ((d (gcd n m)))
    (format #t "\nGeneral pattern: gcd(~a,~a) = ~a\n" n m d)
    (format #t "When we add ~a hours in a ~a-hour clock:\n" m n)
    (cond
     ((= d 1) 
      (format #t "  No fixed points (gcd is 1)\n"))
     ((= d n) 
      (format #t "  All hours are fixed points (clock modulus divides the added value)\n"))
     (else
      (format #t "  Fixed points are at: ")
      (for-each (lambda (i) (format #t "~a " (* i (/ n d))))
                (iota d))
      (format #t "\n  (multiples of ~a up to ~a)\n" (/ n d) n)))))

(explain-fixed-point-pattern 12 12)
(explain-fixed-point-pattern 12 6)
(explain-fixed-point-pattern 12 4)
(explain-fixed-point-pattern 12 5)
(explain-fixed-point-pattern 24 12)
