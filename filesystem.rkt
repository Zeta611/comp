#lang racket
(provide directory-list-with-exts)

(define (directory-list-with-exts . exts)
  (filter
    (lambda (path)
      (memf (curry path-has-extension? path)
            exts))
    (directory-list)))
