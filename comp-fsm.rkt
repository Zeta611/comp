#lang racket
(require "fsm.rkt")
(provide make-comp-fsm)

(define (comp-transition state input)
  (match (cons state input)
    [(cons 'init '@begin-doc) 'doc]
    [(cons 'init (or '@end-doc '@end-doc-eof '@show))
     (error "Invalid transition -- CODE-TRANSITION" state input)]
    [(cons 'init '@hide) 'hidden]
    [(cons 'init text) 'shown]
    [(cons 'init '@eof) 'fini]

    [(cons 'doc (or '@begin-doc '@show '@hide '@eof))
     (error "Invalid transition -- CODE-TRANSITION" state input)]
    [(cons 'doc '@end-doc) 'shown]
    [(cons 'doc '@end-doc-eof) 'fini]
    [(cons 'doc text) 'doc]

    [(cons 'shown (or '@end-doc '@end-doc-eof '@show))
     (error "Invalid transition --  CODE-TRANSITION" state input)]
    [(cons 'shown '@begin-doc) 'doc]
    [(cons 'shown '@hide) 'hidden]
    [(cons 'shown '@eof) 'fini]
    [(cons 'shown text) 'shown]

    [(cons 'hidden (or '@end-doc '@end-doc-eof '@hide))
     (error "Invalid transition -- CODE-TRANSITION" state input)]
    [(cons 'hidden '@begin-doc) 'doc]
    [(cons 'hidden '@show) 'shown]
    [(cons 'hidden '@eof) 'fini]
    [(cons 'hidden text) 'hidden]))

(define (make-comp-fsm transform)
  (make-fsm 'init comp-transition transform))
