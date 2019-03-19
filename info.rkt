#lang info

(define collection "texmath")

(define deps
  '("base"
    "functional-lib"
    "megaparsack-lib"
    "scribble-lib"))

(define build-deps
  '("at-exp-lib"
    "scribble-lib"
    "rackunit-lib"))

(define scribblings '(("scribblings/main.scrbl" () (library) "texmath")))
