#lang racket/base

(require syntax/parse/define
         (only-in scribble/base ~ hspace tabular)
         (for-syntax racket/base))

(provide inferrule)

(begin-for-syntax
  (define (make-rule-premise top)
    #`(tabular #:sep (hspace 3) (list #,top)))

  (define (make-rule top bottom)
    #`(tabular #:row-properties '((center vcenter) (center vcenter top-border))
               (list (list #,(make-rule-premise top))
                     (list #,bottom))))

  (define (make-named-rule top bottom name)
    #`(tabular #:sep (hspace 1)
               #:column-properties '(vcenter)
               (list (list #,(make-rule top bottom) #,name))))

  (define-syntax-class dashes
    #:description "dashes"
    #:attributes ()
    (pattern :id
             #:when (andmap (λ (c) (char=? c #\-))
                            (string->list
                             (symbol->string (syntax->datum this-syntax)))))))

(define-syntax-parser inferrule
  [(_ premise:expr ... :dashes rule-name:expr conclusion:expr)
   (make-named-rule #'(list premise ...) #'conclusion #'rule-name)]
  [(_ premise:expr ...+ :dashes conclusion:expr)
   (make-rule #'(list premise ...) #'conclusion)]
  [(_ :dashes conclusion:expr) (make-rule #'(list ~) #'(elem conclusion))]
  [(_ conclusion:expr) #'(elem conclusion)])
