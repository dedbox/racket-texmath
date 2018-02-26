#lang racket/base

(require (for-syntax racket/base
                     scribble/base)
         scribble/base)

(provide (all-defined-out))

(define-for-syntax (make-premise top)
  #`(tabular #:sep (hspace 2) (list #,top)))

(define-for-syntax (make-rule top bottom)
  #`(tabular #:row-properties '((center vcenter) (center vcenter top-border))
             (list (list #,(make-premise top))
                   (list #,bottom))))

(define-for-syntax (make-named-rule top bottom name)
  #`(tabular #:sep (hspace 1)
             #:column-properties '(vcenter)
             (list (list #,(make-rule top bottom) #,name))))

(define-syntax (inferrule stx)
  (syntax-case stx (---)

    [(_ p premise ... --- rule-name conclusion)
     (make-named-rule #'(list p premise ...) #'conclusion #'rule-name)]

    [(_ p premise ... --- conclusion)
     (make-named-rule #'(list p premise ...) #'conclusion #'"")]

    [(_ --- rule-name conclusion)
     (make-named-rule #'(list ~) #'conclusion #'rule-name)]

    [(_ --- conclusion)
     (make-rule #'(list ~) #'conclusion)]

    [(_ conclusion) #'(elem conclusion)]))