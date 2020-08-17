#lang racket
(provide make-fsm get-state get-transition step run-fsm)

; `transition` is a procedure that returns a pair of the next state and output
; when the current state and an input is given.
(define (make-fsm initial-state transition)
  (cons initial-state transition))

(define get-state car)
(define get-transition cdr)

(define (set-state fsm next-state)
  (make-fsm next-state
            (get-transition fsm)))

; Returns a pair of a next state and an output.
(define (step fsm input)
  (let ((state (get-state fsm))
        (transition (get-transition fsm)))
    (transition state input)))

; Returns a list of outputs processed from `inputs`.
(define (run-fsm fsm inputs)
  (if (null? inputs)
    '()
    (let* ((pair (step fsm (car inputs)))
           (next-state (car pair))
           (output (cdr pair)))
      (cons output
            (run-fsm
              (set-state fsm next-state)
              (cdr inputs))))))

; TEST CODE
(define (code-trans state input)
  (match (cons state input)
    [(cons 'doc (or '@begin-doc '@show '@hide))
     (error "Invalid transition -- CODE-TRANS" state input)]
    [(cons 'doc '@end-doc)
     (cons 'shown '@end-doc)]
    [(cons 'doc text)
     (cons 'doc text)]
    [(cons 'shown (or '@end-doc '@show))
     (error "Invalid transition -- CODE-TRANS" state input)]
    [(cons 'shown '@begin-doc)
     (cons 'doc '@begin-doc)]
    [(cons 'shown '@hide)
     (cons 'hidden '@hide)]
    [(cons 'shown text)
     (cons 'shown text)]
    [(cons 'hidden (or '@end-doc '@hide))
     (error "Invalid transition -- CODE-TRANS" state input)]
    [(cons 'hidden '@begin-doc)
     (cons 'doc '@begin-doc)]
    [(cons 'hidden '@show)
     (cons 'shown '@show)]
    [(cons 'hidden text)
     (cons 'hidden text)]))

(define sample-fsm
  (make-fsm 'shown code-trans))

(display
  (run-fsm
    sample-fsm
    (list '@hide '@show "(define x 1)" '@begin-doc "Hello, Doc!" '@end-doc)))
(newline)
