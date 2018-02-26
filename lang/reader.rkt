#lang racket/base

(require syntax/module-reader
         (only-in texmath/reader make-texmath-readtable))

(provide (rename-out [texmath-read read]
                     [texmath-read-syntax read-syntax]
                     [texmath-get-info get-info]))

(define (wrap-reader p)
  (lambda args
    (parameterize ([current-readtable (make-texmath-readtable)])
      (apply p args))))

(define-values (texmath-read texmath-read-syntax texmath-get-info)
  (make-meta-reader
   'texmath
   "language path"
   lang-reader-module-paths
   wrap-reader
   (lambda (orig-read-syntax)
     (define read-syntax (wrap-reader orig-read-syntax))
     (lambda args
       (define stx (apply read-syntax args))
       (define old-prop (syntax-property stx 'module-language))
       (define new-prop `#(texmath/lang/language-info get-language-info ,old-prop))
       (syntax-property stx 'module-language new-prop)))
   (lambda (proc)
     (lambda (key defval)
       (define (fallback) (if proc (proc key defval) defval))
       (define (try-dynamic-require lib export)
         (with-handlers ([exn:missing-module? (λ (x) (fallback))])
           (dynamic-require lib export)))
       (case key
         [(color-lexer)
          (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
         [(drracket:indentation)
          (try-dynamic-require 'scribble/private/indentation 'determine-spaces)]
         [(drracket:keystrokes)
          (try-dynamic-require 'scribble/private/indentation 'keystrokes)]
         [else (fallback)])))))
