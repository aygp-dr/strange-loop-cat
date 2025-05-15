;; Dihedral group operations for D_n
(define (make-dihedral-group n)
  ;; Elements are represented as pairs (i, j) where:
  ;; - i is the rotation index (0 to n-1)
  ;; - j is 0 for rotation, 1 for reflection
  
  ;; Identity element
  (define identity '(0 0))
  
  ;; Group operation (multiplication)
  (define (multiply a b)
    (let ((a-rot (car a))
          (a-ref (cadr a))
          (b-rot (car b))
          (b-ref (cadr b)))
      (if (= a-ref 0)
          ;; a is a rotation
          (list (modulo (+ a-rot b-rot) n) b-ref)
          ;; a is a reflection
          (list (modulo (- a-rot b-rot) n) (modulo (+ a-ref b-ref) 2)))))
  
  ;; Get all elements of the group
  (define (elements)
    (append
     ;; Rotations
     (map (lambda (i) (list i 0)) (iota n))
     ;; Reflections
     (map (lambda (i) (list i 1)) (iota n))))
  
  ;; Find fixed points of a group action
  (define (fixed-points element)
    (filter
     (lambda (point)
       (equal? point (multiply element point)))
     (elements)))
  
  ;; Return the group interface
  (lambda (message . args)
    (case message
      ((elements) (elements))
      ((multiply) (apply multiply args))
      ((identity) identity)
      ((fixed-points) (apply fixed-points args))
      (else (error "Unknown message" message)))))

;; Create and analyze dihedral groups
(define d4 (make-dihedral-group 4))  ;; Square symmetry / compass directions
(define d12 (make-dihedral-group 12)) ;; Dodecagon symmetry / musical notes

;; Show examples
(format #t "Dihedral group D4 (square symmetry / compass directions):\n")
(format #t "  Elements: ~a\n" (d4 'elements))
(format #t "  D4 identity: ~a\n" (d4 'identity))
(format #t "  Example multiplication: ~a\n" 
        (d4 'multiply '(1 0) '(2 1)))

(format #t "\nD4 fixed points analysis:\n")
(let ((rot-90 '(1 0))     ;; 90° rotation
      (rot-180 '(2 0))    ;; 180° rotation
      (reflection '(0 1))) ;; Reflection
  (format #t "  Fixed points of 90° rotation: ~a\n" 
          (d4 'fixed-points rot-90))
  (format #t "  Fixed points of 180° rotation: ~a\n" 
          (d4 'fixed-points rot-180))
  (format #t "  Fixed points of reflection: ~a\n" 
          (d4 'fixed-points reflection)))

(format #t "\nDihedral group D12 (dodecagon symmetry / musical notes):\n")
(format #t "  Fixed points of tritone (6-step rotation): ~a\n" 
        (d12 'fixed-points '(6 0)))
(format #t "  Fixed points of perfect fifth (7-step rotation): ~a\n" 
        (d12 'fixed-points '(7 0)))
