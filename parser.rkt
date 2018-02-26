#lang racket/base

(require parser-tools/lex
         parser-tools/yacc
         racket/function
         scribble/base
         scribble/core
         syntax/readerr
         texmath/lexer)

(provide (all-defined-out))

;; (define (intersperse sep xs)
;;   (if (null? xs)
;;       null
;;       (cons (car xs) (prepend-to-all sep (cdr xs)))))

;; (define (prepend-to-all sep xs)
;;   (if (null? xs)
;;       null
;;       (cons sep (cons (car xs) (prepend-to-all sep (cdr xs))))))

(define (texmath-parser)
  (parser
   (src-pos)
   (start s)
   (end EOF)
   (error (λ (a name val start end)
            (raise-read-error "read-error"
                              "texmath"
                              (position-line start)
                              (position-col start)
                              (position-offset start)
                              (- (position-offset end)
                                 (position-offset start)))))
   (tokens words delim)

   (grammar
    (s [(expr-list) (apply elem (reverse $1))])

    (expr-list
     [() null]
     [(expr-list expr) (cons (hspace 1) (cons (apply elem (reverse $2)) $1))])

    (expr
     [(expr UNDERSCORE atoms) (cons (apply subscript $3) $1)]
     [(expr CARAT atoms) (cons (apply superscript $3) $1)]
     [(expr PRIME) (cons (superscript (larger (bold (element 'tt "'")))) $1)]
     [(atoms) $1])

    (atoms
     [(atom) (list $1)]
     [(LEFT-BRACE atom-list RIGHT-BRACE) (reverse $2)])

    (atom-list
     [() null]
     [(atom-list atom) (cons $2 $1)])

    (atom
     [(WORD) (italic $1)]
     [(SYM) $1]))))

(define (texmath-parse in)
  (port-count-lines! in)
  ((texmath-parser) (λ () (texmath-lexer in))))

(define (texmath-parse-string str)
  (texmath-parse (open-input-string str)))
