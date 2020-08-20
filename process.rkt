#lang racket
(provide process)

(define (process contents)
  (define (doc-process contents)
    (define (inner contents mode)
      (define (handle-chars mode chars)
        (define (helper mode tl chars-to-append)
          (let-values ([(final-mode chars) (handle-chars mode tl)])
            (values final-mode
                    (append chars-to-append chars))))
        (match (cons mode chars)
          [(cons _ '()) (values mode '())]
          [(cons 'normal (list #\$ tl ...))
           (helper 'math tl (list #\$))]
          [(cons 'normal (or (list #\* #\* tl ...)
                             (list #\_ #\_ tl ...)))
           (helper 'strong tl (string->list "<strong>"))]
          [(cons 'normal (list (or #\* #\_) tl ...))
           (helper 'emph tl (string->list "<em>"))]
          [(cons 'normal (list #\` tl ...))
           (helper 'code tl (string->list "<code>"))]
          [(cons 'math (list #\$ tl ...))
           (helper 'normal tl (list #\$))]
          [(cons 'strong (or (list #\* #\* tl ...)
                             (list #\_ #\_ tl ...)))
           (helper 'normal tl (string->list "</strong>"))]
          [(cons 'emph (list (or #\* #\_) tl ...))
           (helper 'normal tl (string->list "</em>"))]
          [(cons 'code (list #\` tl ...))
           (helper 'normal tl (string->list "</code>"))]
          [(cons mode (list hd tl ...))
           (helper mode tl (list hd))]))
      (match contents
        ['() (list "</p>")]
        [(list '@end-doc tl ...)
         (cons "</p>" (process tl))]
        [(list hd tl ...)
         (let*-values ([(mode chars)
                        (handle-chars mode (string->list hd))])
           (cons (list->string chars)
                 (inner tl mode)))]))
    (cons "<p>" (inner contents 'normal)))

  (define (hide-process contents)
    (match contents
      ['() '()]
      [(list '@end-hide tl ...) (process tl)]
      [(list hd tl ...) (hide-process tl)]))

  (define (code-process contents)
    (define (inner contents)
      (match contents
        ['()
         (list "</code></p>")]
        [(list symb tl ...)
         #:when (symbol? symb)
         (cons "</code></p>" (process contents))]
        [(list hd tl ...)
         (cons hd (inner tl))]))
    (cons "<p><code>" (inner contents)))

  (match contents
    ['() '()]
    [(list '@begin-doc tl ...) (doc-process tl)]
    [(list '@begin-hide tl ...) (hide-process tl)]
    [else (code-process contents)]))

; TEST CODE
(display
  (process
    (list '@begin-doc
          "Some doc"
          "$F = ma$ *em"
          "ph* __strong"
          "__."
          '@end-doc
          "(define shown 0)"
          '@begin-hide
          "(define hidden 1)"
          "(define hidden2 2)"
          '@end-hide
          "(define final x)"
          '@begin-doc
          "FOO"
          '@end-doc)))
(newline)
