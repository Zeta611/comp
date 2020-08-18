#lang racket
(require "fsm.rkt")
(provide make-comp-fsm)

(define (comp-transition state input)
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

(define (make-comp-fsm transform)
  (make-fsm 'shown comp-transition transform))
