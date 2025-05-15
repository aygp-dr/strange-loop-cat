;; Define musical notes as integers (C=0, C#=1, ... B=11)
(define-record-type <musical-phrase>
  (make-musical-phrase notes durations)
  musical-phrase?
  (notes phrase-notes)
  (durations phrase-durations))

;; Musical transformations from GEB
(define (transpose phrase semitones)
  (make-musical-phrase
   (map (lambda (note) (modulo (+ note semitones) 12))
        (phrase-notes phrase))
   (phrase-durations phrase)))

(define (invert phrase)
  (let ((first-note (car (phrase-notes phrase))))
    (make-musical-phrase
     (map (lambda (note) (modulo (- (* 2 first-note) note) 12))
          (phrase-notes phrase))
     (phrase-durations phrase))))

(define (retrograde phrase)
  (make-musical-phrase
   (reverse (phrase-notes phrase))
   (reverse (phrase-durations phrase))))

(define (augment phrase factor)
  (make-musical-phrase
   (phrase-notes phrase)
   (map (lambda (dur) (* dur factor))
        (phrase-durations phrase))))

(define (canonic-transformation phrase)
  (let* ((transposed (transpose phrase 7))        ;; Up a fifth
         (inverted (invert transposed))          ;; Then invert
         (augmented (augment inverted 2)))       ;; Double the duration
    augmented))

;; Theme from Bach's Musical Offering (simplified)
(define royal-theme
  (make-musical-phrase
   '(0 2 3 5 7 8 7 5 4 0)  ;; C D D# F G G# G F E C
   '(1 1 1 1 1 1 1 1 1 2)))

;; Apply transformations
(define transformed-theme (canonic-transformation royal-theme))

;; Display results in a GEB-inspired format
(define (display-theme phrase)
  (define note-names '#("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
  (for-each (lambda (note dur)
              (format #t "~a(~a) " 
                      (vector-ref note-names note)
                      dur))
            (phrase-notes phrase)
            (phrase-durations phrase))
  (newline))

(format #t "Original theme: ")
(display-theme royal-theme)
(format #t "Transformed theme: ")
(display-theme transformed-theme)
