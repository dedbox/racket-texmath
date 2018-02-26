#lang racket/base

(require (only-in scribble/base elem italic subscript superscript larger tt)
         syntax/readerr
         (for-syntax texmath)
         texmath/parser)

(provide $-read
         $-read-syntax
         make-$-readtable
         make-bar-readtable
         make-texmath-readtable)

(define ($-read in)
  (syntax->datum ($-read-syntax #f in)))

(define ($-read-syntax src in)
  (define-values (line col pos) (port-next-location in))
  (define matched (regexp-match #px"^([^$]+)\\$" in))
  (unless matched
    (raise-read-error "bad texmath syntax"
                      src line col pos
                      (and pos (- (file-position in) pos))))
  (define expr-str (bytes->string/utf-8 (cadr matched)))
  (datum->syntax
   #f
   (texmath-parse-string expr-str)
   (and line (vector src line col pos (string-length expr-str)))))

(define (make-$-readtable)
  (make-readtable (current-readtable) #\$ 'terminating-macro read-dollar))

(define read-dollar
  (case-lambda
    [(ch in) ($-read in)]
    [(ch in src line col pos) ($-read-syntax src in)]))

(define (bar-read in)
  (syntax->datum (bar-read-syntax #f in)))

(define (bar-read-syntax src in)
  (define-values (line col pos) (port-next-location in))
  (define dash-count
    (let loop ([n 1])
      (if (equal? (peek-bytes 1 0 in) #"-")
          (begin (read-byte in) (loop (add1 n)))
          n)))
  (datum->syntax
   #f
   (cond [(>= dash-count 3) '---]
         [(= dash-count 2) '--]
         [(= dash-count 1) '-])
   (and line (vector src line col pos dash-count))))

(define (make-bar-readtable)
  (make-readtable (current-readtable) #\- 'terminating-macro read-bar))

(define read-bar
  (case-lambda
    [(ch in) (bar-read in)]
    [(ch in src line col pos) (bar-read-syntax src in)]))

(define (make-texmath-readtable)
  (make-readtable
   (make-readtable (current-readtable) #\$ 'terminating-macro read-dollar)
   #\- 'terminating-macro read-bar))
