;; A real-world example of a strange loop: a thermostat system
;; - Thermostat senses room temperature
;; - Thermostat compares to desired temperature
;; - Thermostat controls heater
;; - Heater changes room temperature
;; - Room temperature is sensed by thermostat (loop back to start)

(define (make-thermostat-system)
  (let ((room-temp 65)
        (desired-temp 70)
        (heater-on #f))
    
    ;; Define the state transformation
    (define (next-state)
      (set! heater-on (< room-temp desired-temp))
      (if heater-on
          (set! room-temp (+ room-temp 1))
          (set! room-temp (- room-temp 0.5)))
      (list room-temp desired-temp heater-on))
    
    ;; Return functions to interact with the system
    (list
     (lambda () (list room-temp desired-temp heater-on))  ; get-state
     (lambda (new-temp) (set! desired-temp new-temp))     ; set-desired-temp
     next-state)))                                        ; advance-system

;; Create a thermostat system
(define system (make-thermostat-system))
(define get-state (car system))
(define set-desired-temp (cadr system))
(define advance-system (caddr system))

;; Simulate the system for a few steps
(display "Initial state: ") (display (get-state)) (newline)

(display "Advancing system...\n")
(dotimes (i 10)
  (display "Step ") (display i) (display ": ")
  (display (advance-system))
  (newline))

;; Change desired temperature and observe system response
(display "\nChanging desired temperature to 65 degrees\n")
(set-desired-temp 65)

(display "Advancing system with new target...\n")
(dotimes (i 10)
  (display "Step ") (display i) (display ": ")
  (display (advance-system))
  (newline))
