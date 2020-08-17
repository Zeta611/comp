#! /usr/bin/env racket
#lang racket

(define filename
  (command-line
    #:program "COMP"
    #:args (filename)
    filename))

; comment strings
(define begin-block-comment "#|")
(define end-block-comment "|#")
(define line-comment ";")

; directives
(define begin-doc "@begin-doc")
(define end-doc "@end-doc")
(define hide "@hide")
(define show "@show")

(define cbegin-doc
  (string-append begin-block-comment
                 begin-doc))
(define end-docc
  (string-append end-doc
                 end-block-comment))
(define chide
  (string-append line-comment
                 hide))
(define cshow
  (string-append line-comment
                 show))


(define (generate-code lines)
  (define (helper lines strip?)
    (if (null? lines)
        '()
        (let ((hd (car lines))
              (tl (cdr lines)))
          (if strip?
              (cond ((equal? hd end-docc)
                     (helper tl #f))
                    ((equal? hd cshow)
                     (helper tl #f))
                    (else
                      (helper tl #t)))
              (cond ((equal? hd cbegin-doc)
                     (helper tl #t))
                    ((equal? hd chide)
                     (helper tl #t))
                    (else
                      (cons hd (helper tl #f))))))))
  (helper lines #f))

(define (print-each lines)
  (for-each (curry printf "~a\n")
            lines))

(print-each
  (generate-code
    (file->lines filename)))
