;; Define a 2D point
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define (point=? p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

;; Mirror reflections
(define (reflect-x point)
  (make-point (point-x point) (- (point-y point))))

(define (reflect-y point)
  (make-point (- (point-x point)) (point-y point)))

(define (reflect-origin point)
  (make-point (- (point-x point)) (- (point-y point))))

(define (rotate-180 point)
  (reflect-origin point))

;; Check for fixed points
(define (fixed-point? transform point)
  (point=? point (transform point)))

;; Analyze some points
(define test-points
  (list (make-point 3 4)    ;; Regular point
        (make-point 5 0)    ;; Point on x-axis
        (make-point 0 7)    ;; Point on y-axis
        (make-point 0 0)))  ;; Origin

(define (analyze-reflection-fixed-points)
  (for-each
   (lambda (point)
     (format #t "Point (~a,~a):\n" (point-x point) (point-y point))
     (format #t "  Fixed point of x-reflection? ~a\n" 
             (fixed-point? reflect-x point))
     (format #t "  Fixed point of y-reflection? ~a\n" 
             (fixed-point? reflect-y point))
     (format #t "  Fixed point of origin-reflection? ~a\n" 
             (fixed-point? reflect-origin point)))
   test-points))

(analyze-reflection-fixed-points)
