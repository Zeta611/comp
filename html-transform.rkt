#lang racket
(provide html-transform)

(define (html-transform curr-state next-state)
  (match (cons curr-state next-state)
    [(cons (or 'init 'hidden)
           'doc)
     (thunk* "<p>")]
    [(cons (or 'init 'hidden)
           'shown)
     (thunk* "<p><code>")]
    [(cons (or 'init 'hidden)
           (or 'hidden 'fini))
     (thunk* 'null)]
    [(cons 'doc 'doc)
     identity] ; TODO
    [(cons 'doc 'shown)
     (thunk* "</p><p><code>")]
    [(cons 'doc 'hidden)
     (error "Invalid transform -- CODE-TRANSFORM" curr-state next-state)]
    [(cons 'doc 'fini)
     (thunk* "</p>")]
    [(cons 'shown 'doc)
     (thunk* "</code></p><p>")]
    [(cons 'shown 'shown)
     identity]
    [(cons 'shown (or 'hidden 'fini))
     (thunk* "</code></p>")]))

; TEST CODE
(require "fsm.rkt" "comp-fsm.rkt")

(define machine
  (make-comp-fsm html-transform))

(display
  (run-fsm machine
           (list '@begin-doc
                 "Some doc"
                 '@end-doc
                 "(define shown 0)"
                 '@hide
                 "(define hidden 1)"
                 "(define hidden2 2)"
                 '@show
                 "(define final x)"
                 '@begin-doc
                 "FOO"
                 '@end-doc-eof)))
(newline)
