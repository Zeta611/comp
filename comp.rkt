#! /usr/bin/env racket
#lang racket
(require "preprocess.rkt" "process.rkt" "postprocess.rkt")

(define filename
  (command-line
    #:program "COMP"
    #:args (filename)
    filename))

(define (comp file)
  (let ((contents (file->lines file)))
    (postprocess
      (process
        (preprocess contents)))))

(display (comp filename))
(newline)
