#lang racket
(provide preprocess)

(define (preprocess contents)
  (map
    (lambda (str)
      (case str
        [("#|@begin-doc") '@begin-doc]
        [("@end-doc|#") '@end-doc]
        [(";@begin-hide") '@begin-hide]
        [(";@end-hide") '@end-hide]
        [else str]))
    contents))
