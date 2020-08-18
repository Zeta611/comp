#lang racket
(provide make-fsm run-fsm)

; `transition` is a procedure that returns a next state when the current state
; and an input is given, and 'transform' is a procedure that returns an input
; processor when the current state and the next state is given.
(define (make-fsm initial-state transition transform)
  (cons initial-state (cons transition transform)))

(define get-state car)
(define get-transition cadr)
(define get-transform cddr)

; Returns an fsm with its state replaced with `next-state`.
(define (set-state fsm next-state)
  (make-fsm next-state
            (get-transition fsm)
            (get-transform fsm)))

; Returns a pair of a next state and an output of `fsm` when `input` is given.
(define (step fsm input)
  (let ((transition (get-transition fsm))
        (transform (get-transform fsm))
        (curr-state (get-state fsm)))
    (let* ((next-state (transition curr-state input))
           (output ((transform curr-state next-state) input)))
      (cons next-state output))))

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
(define (code-transition state input)
  (match (cons state input)
    [(cons 'doc (or '@begin-doc '@show '@hide))
     (error "Invalid transition -- CODE-TRANSITION" state input)]
    [(cons 'doc '@end-doc) 'shown]
    [(cons 'doc text) 'doc]

    [(cons 'shown (or '@end-doc '@show))
     (error "Invalid transition --  CODE-TRANSITION" state input)]
    [(cons 'shown '@begin-doc) 'doc]
    [(cons 'shown '@hide) 'hidden]
    [(cons 'shown text) 'shown]

    [(cons 'hidden (or '@end-doc '@hide))
     (error "Invalid transition -- CODE-TRANSITION" state input)]
    [(cons 'hidden '@begin-doc) 'doc]
    [(cons 'hidden '@show) 'shown]
    [(cons 'hidden text) 'hidden]))

(define (code-transform curr-state next-state)
  (match (cons curr-state next-state)
    [(cons s s)
     identity]
    [(cons 'doc 'shown)
     identity]
    [(cons 'doc 'hidden)
     (error "Invalid transform -- CODE-TRANSFORM" curr-state next-state)]
    [(cons 'shown 'doc)
     identity]
    [(cons 'shown 'hidden)
     identity]
    [(cons 'hidden 'doc)
     identity]
    [(cons 'hidden 'shown)
     identity]))

(define sample-fsm
  (make-fsm 'shown code-transition code-transform))

(display
  (run-fsm
    sample-fsm
    (list '@hide '@show "(define x 1)" '@begin-doc "Hello, Doc!" '@end-doc)))
(newline)
