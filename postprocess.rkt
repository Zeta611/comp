#lang racket
(provide postprocess)

(define (postprocess contents)
  (define html-start
    (string-join
      (list "<!DOCTYPE html>"
            "<html lang=\"en-US\">"
            "<head>"
            "<title>"
            "TEST"
            "</title>"
            "</head>"
            "<body>")
      "\n"))
  (define html-end "</body>\n</html>")
  (string-append
      (string-join
        (cons html-start contents)
        "\n")
      "\n"
      html-end))
