#lang info

(define collection "texmath")

(define deps '("base"
               "functional-lib"
               "megaparsack-lib"
               "scribble-lib"))

(define build-deps '("racket-doc"
                     "scribble-doc"))

(define scribblings '(("scribblings/main.scrbl" () (library) "texmath")))
