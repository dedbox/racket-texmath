#lang info

(define collection "texmath")

(define deps
  '("base"
    "parser-tools-lib"
    "scribble-lib"))

(define build-deps
  '("at-exp-lib"
    "scribble-lib"))

(define scribblings '(("scribblings/main.scrbl" () (library) "texmath")))
