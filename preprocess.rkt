#lang racket
(provide preprocess)

(define (preprocess contents)
  (map
    (lambda (str)
      (let ((str (string-trim str)))
        (case str
          [("#|@begin-doc") '@begin-doc]
          [("@end-doc|#") '@end-doc]
          [(";@begin-hide") '@begin-hide]
          [(";@end-hide") '@end-hide]
          [else str])))
    contents))

; TEST CODE
(display
  (preprocess
    (list "#|@begin-doc"
          "Some doc"
          "$F = ma$ *em"
          "ph* __strong"
          "__."
          "@end-doc|#"
          "(define shown 0)"
          ";@begin-hide"
          "(define hidden 1)"
          "(define hidden2 2)"
          ";@end-hide"
          "(define final x)"
          "#|@begin-doc"
          "FOO"
          "@end-doc|#")))
(newline)
