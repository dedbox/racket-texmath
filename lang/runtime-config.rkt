#lang racket/base

(provide configure)

(require (only-in texmath/reader make-$-readtable))

(define (configure data)
  (define old-read (current-read-interaction))
  (define (new-read src in)
    (parameterize ([current-readtable (make-$-readtable)])
      (old-read src in)))
  (current-read-interaction new-read))