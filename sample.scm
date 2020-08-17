#|@begin-doc
This is a document.
@end-doc|#
(define shown #t)
;@hide
(define not-shown
  (+ 0 1 2))
;@show
(define (foo x)
  (+ 1 x))
