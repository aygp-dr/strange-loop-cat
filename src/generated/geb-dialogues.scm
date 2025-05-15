;; GEB Dialogue structure implemented in Scheme
(define-record-type <character>
  (make-character name personality)
  character?
  (name character-name)
  (personality character-personality))

(define-record-type <dialogue>
  (make-dialogue title characters script)
  dialogue?
  (title dialogue-title)
  (characters dialogue-characters)
  (script dialogue-script))

;; Define the characters
(define achilles
  (make-character 
   "Achilles" 
   "Naive but curious, quick to accept new ideas"))

(define tortoise
  (make-character 
   "Tortoise"
   "Wise, methodical, fond of logical paradoxes"))

;; Create a dialogue about strange loops
(define strange-loop-dialogue
  (make-dialogue
   "Strange Loops and Monomorphisms"
   (list achilles tortoise)
   (list
    (cons achilles "I've been reading about these category theory concepts, but I'm struggling with monomorphisms and epimorphisms.")
    (cons tortoise "Let me explain with a simple example. Consider a record player.")
    (cons achilles "A record player?")
    (cons tortoise "Yes! When you play a vinyl record, the needle traces the grooves and transforms physical patterns into sound waves. This is a monomorphism.")
    (cons achilles "How so?")
    (cons tortoise "Because different groove patterns always produce different sounds. If two records produce exactly the same sound in your player, they must have identical grooves!")
    (cons achilles "I see! And what about epimorphisms?")
    (cons tortoise "Consider the volume knob on that same record player. It maps a continuous rotation to a range of volumes from silent to maximum.")
    (cons achilles "And that's an epimorphism because...?")
    (cons tortoise "Because every possible volume level can be reached by some position of the knob. No matter what volume you want, there's a knob position that gets you there!"))))

;; Display the dialogue in GEB style
(define (display-dialogue dialogue)
  (format #t "~a\n\n" (dialogue-title dialogue))
  (for-each
   (lambda (line)
     (format #t "~a: ~a\n\n" 
             (character-name (car line)) 
             (cdr line)))
   (dialogue-script dialogue)))

(display-dialogue strange-loop-dialogue)
