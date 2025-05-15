;; Define compass directions
(define directions #("North" "East" "South" "West"))

(define (direction-name dir)
  (vector-ref directions (modulo dir 4)))

;; Direction transformations
(define (turn-right dir)
  (modulo (+ dir 1) 4))

(define (turn-left dir)
  (modulo (- dir 1) 4))

(define (turn-around dir)
  (modulo (+ dir 2) 4))

;; Helper to apply a function multiple times
(define (iterate n f x)
  (if (= n 0)
      x
      (iterate (- n 1) f (f x))))

;; Explore fixed points 
(define (explore-direction-transformations)
  (for-each 
   (lambda (dir)
     (format #t "Starting from ~a:\n" (direction-name dir))
     (format #t "  Turn right 4 times: ~a\n" 
             (direction-name (iterate 4 turn-right dir)))
     (format #t "  Turn left 4 times: ~a\n" 
             (direction-name (iterate 4 turn-left dir)))
     (format #t "  Turn around twice: ~a\n" 
             (direction-name (iterate 2 turn-around dir))))
   (iota 4)))

(explore-direction-transformations)
