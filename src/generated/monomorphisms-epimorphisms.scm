;; GEB-inspired demonstration of monomorphisms and epimorphisms through types
(define-record-type <note>
  (make-note name octave)
  note?
  (name note-name)
  (octave note-octave))

;; Monomorphism: Each distinct note maps to a unique frequency
(define (note->frequency note)
  (let* ((base-a4 440.0)
         (note-values '((C . 0) (D . 2) (E . 4) (F . 5) 
                         (G . 7) (A . 9) (B . 11)))
         (semitones-from-a4 
          (+ (* 12 (- (note-octave note) 4))
             (- (cdr (assoc (note-name note) note-values)) 9))))
    (* base-a4 (expt 2 (/ semitones-from-a4 12)))))

;; Epimorphism: Every century is hit by some year
(define-record-type <album>
  (make-album title artist year)
  album?
  (title album-title)
  (artist album-artist)
  (year album-year))

(define (album->century album)
  (+ 1 (quotient (album-year album) 100)))

;; GEB-style demonstration
(define notes
  (list (make-note 'A 4) 
        (make-note 'C 5)
        (make-note 'E 5)))

(define albums
  (list (make-album "The Goldberg Variations" "Bach" 1741)
        (make-album "The Art of Fugue" "Bach" 1750)
        (make-album "Nocturnes" "Chopin" 1832)
        (make-album "Thus Spoke Zarathustra" "Strauss" 1896)))

;; Demonstrate monomorphism (injective mapping)
(format #t "Monomorphism (Note -> Frequency):\n")
(for-each 
 (lambda (note) 
   (format #t "~a~a: ~a Hz\n" 
           (note-name note) 
           (note-octave note)
           (note->frequency note)))
 notes)

(format #t "\nEach distinct note has a distinct frequency.\n")
(format #t "If f(a) = f(b), then a = b (injective property)\n\n")

;; Demonstrate epimorphism (surjective mapping)
(format #t "Epimorphism (Album -> Century):\n")
(for-each
 (lambda (album)
   (format #t "~a (~a): ~ath century\n"
           (album-title album)
           (album-year album)
           (album->century album)))
 albums)

(format #t "\nEvery relevant century is hit by some album.\n")
(format #t "For every output, there exists an input mapping to it.\n")
