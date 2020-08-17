#lang racket
(provide make-fsm get-state get-transition step run-fsm)

; transition is a procedure that returns the next state when state and input
; is given.
(define (make-fsm initial-state transition)
  (cons initial-state transition))

(define get-state car)
(define get-transition cdr)

(define (set-state fsm next-state)
  (make-fsm next-state
            (get-transition fsm)))

(define (step fsm input)
  (let ((state (get-state fsm))
        (transition (get-transition fsm)))
    (transition state input)))

(define (run-fsm fsm inputs)
  (unless (null? inputs)
    (let ((next-state (step fsm (car inputs))))
      (run-fsm 
        (set-state fsm next-state)
        (cdr inputs)))))

; TEST CODE
(define (code-trans state input)
  (display state)
  (newline)
  (match (cons state input)
    [(cons 'doc (or '@begin-doc '@show '@hide))
     (error "Invalid transition -- CODE-TRANS" state input)]
    [(cons 'doc '@end-doc)
     'shown]
    [(cons 'doc 'text)
     'doc]
    [(cons 'shown (or '@end-doc '@show))
     (error "Invalid transition -- CODE-TRANS" state input)]
    [(cons 'shown '@begin-doc)
     'doc]
    [(cons 'shown '@hide)
     'hidden]
    [(cons 'shown 'text)
     'shown]
    [(cons 'hidden (or '@end-doc '@hide))
     (error "Invalid transition -- CODE-TRANS" state input)]
    [(cons 'hidden '@begin-doc)
     'doc]
    [(cons 'hidden '@show)
     'shown]
    [(cons 'hidden 'text)
     'hidden]))

(define sample-fsm
  (make-fsm 'shown code-trans))

(run-fsm sample-fsm (list '@hide '@show))
