;; Define the 12-tone chromatic scale
(define note-names #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

;; Convert note number to name
(define (note-name note-number)
  (vector-ref note-names (modulo note-number 12)))

;; Musical interval operations
(define (transpose note interval)
  (modulo (+ note interval) 12))

(define (interval between-notes)
  (modulo (- (cadr between-notes) (car between-notes)) 12))

;; Display the chromatic scale
(define (display-chromatic-scale)
  (for-each (lambda (i)
              (format #t "~a " (note-name i)))
            (iota 12))
  (newline))

(display "Chromatic scale: ")
(display-chromatic-scale)

;; Define common musical intervals
(define unison 0)
(define semitone 1)
(define tone 2)
(define minor-third 3)
(define major-third 4)
(define perfect-fourth 5)
(define tritone 6)
(define perfect-fifth 7)
(define minor-sixth 8)
(define major-sixth 9)
(define minor-seventh 10)
(define major-seventh 11)
(define octave 12)

;; Find fixed points of a musical transformation
(define (find-fixed-points transformation)
  (filter (lambda (note)
            (= note (transformation note)))
          (iota 12)))

;; Examine different intervals
(define (transpose-by-tritone note)
  (transpose note tritone))

(define (transpose-by-fifth note)
  (transpose note perfect-fifth))

;; Display fixed points
(format #t "Fixed points of tritone transposition: ")
(for-each (lambda (note)
            (format #t "~a " (note-name note)))
          (find-fixed-points transpose-by-tritone))
(newline)

(format #t "Fixed points of perfect fifth transposition: ")
(for-each (lambda (note)
            (format #t "~a " (note-name note)))
          (find-fixed-points transpose-by-fifth))
(newline)

;; Generate the Circle of Fifths
(define (circle-of-fifths starting-note)
  (let loop ((current starting-note)
             (count 0)
             (result '()))
    (if (and (> count 0) (= current starting-note))
        (reverse result)
        (loop (transpose current perfect-fifth)
              (+ count 1)
              (cons current result)))))

(define c-circle (circle-of-fifths 0)) ;; Start from C (0)

(format #t "Circle of Fifths starting from C: ")
(for-each (lambda (note)
            (format #t "~a " (note-name note)))
          c-circle)
(newline)

(format #t "Number of steps to return to C: ~a\n" (length c-circle))
