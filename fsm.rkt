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
