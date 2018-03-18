#lang racket/base

(require data/applicative
         data/either
         data/monad
         megaparsack
         megaparsack/text
         racket/function
         racket/list
         racket/match
         (only-in scribble/base
                  elem italic hspace larger linebreak subscript superscript)
         (only-in scribble/core
                  element))

(require texmath/inferrule)

(provide $ inferrule)

;; IR

(struct Term (base subscript superscript primes) #:transparent)

;; Character Classes

(define symbol-char/p
  (satisfy/p
   (λ (c)
     (and (char-symbolic? c)
          (not (char=? c #\^))))))

(define word-char/p
  (or/p letter/p
        digit/p
        symbol-char/p))

;; Lexical Tokens

(struct TOK (value) #:transparent)
(struct SPC TOK () #:transparent)
(struct RET TOK () #:transparent)
(struct NUM TOK () #:transparent)
(struct LIT TOK () #:transparent)
(struct PNC TOK () #:transparent)
(struct WRD TOK () #:transparent)

(define <SPC>
  ((pure SPC)
   ((pure length)
    (many+/p (satisfy/p
              (λ (c)
                (and (char-whitespace? c)
                     (not (char=? c #\newline)))))))))

(define <RET>
  ((pure RET)
   ((pure length)
    (many+/p (char/p #\newline)))))

(define <SPC+RET>
  (or/p <SPC> <RET>))

(define <NUM>
  ((pure NUM) integer/p))

(define <LIT>
  ((pure LIT)
   ((pure (curry apply string))
    (do (char/p #\\)
        (or/p (list/p (char/p #\{))
              (list/p (char/p #\}))
              (many+/p word-char/p))))))

(define <PNC>
  ((pure PNC)
   (satisfy/p
    (λ (c)
      (and (char-punctuation? c)
           (not (or (char=? #\_ c)
                    (char=? #\{ c)
                    (char=? #\} c))))))))

(define <WRD>
  ((pure WRD)
   (do (c <- (or/p symbol-char/p letter/p))
       (cs <- (many/p word-char/p))
       (pure (apply string (cons c cs))))))

;; Parse Grammar

(define <sub-term>
  (do (char/p #\{)
      (ts <- <term-list>)
      (char/p #\})
      (pure ts)))

(define <atom>
  (or/p <NUM>
        <LIT>
        <PNC>
        <WRD>
        <sub-term>))

(define <subscript>
  (do (char/p #\_) <atom>))

(define <superscript>
  (do (char/p #\^) <atom>))

(define <primes>
  ((pure length)
   (many/p (char/p #\'))))

(define <term>
  ((pure Term)
   <atom>
   (or/p <subscript> (pure #f))
   (or/p <superscript> (pure #f))
   <primes>))

(define <term-list>
  (many/p (or/p <term> <SPC> <RET>)))

(define <input>
  (do (ts <- <term-list>)
      eof/p
      (pure ts)))

;; Parser

(define (parse-term str)
  (parse-result! (parse-string <term> str)))

(define (parse-term-list str)
  (parse-result! (parse-string <term-list> str)))

(define (parse-input str)
  (parse-result! (parse-string <input> str)))

;; Ligatures

(define current-mathligs
  (make-parameter (make-hash)))

(define (mathlig from to)
  (hash-set! (current-mathligs) (parse-term from) to))

(define (get-mathlig from)
  (hash-ref (current-mathligs) from #f))

(mathlig "==>" "⇒")

(mathlig "\\sqrt"
         (λ (ts)
           (values
            (flatten
             (list (parse-term-list "√(")
                   (car ts)
                   (parse-term ")")))
            (cdr ts))))

(define (apply-mathligs args)
  (define (remap t ts)
    (define lig (get-mathlig t))
    (cond [(string? lig) (values (list (parse-term lig)) ts)]
          [(procedure? lig) (lig ts)]
          [else (values (list t) ts)]))
  (if (null? args)
      null
      (call-with-values (λ () (remap (car args) (cdr args)))
        (λ (pre ts) (append pre (apply-mathligs ts))))))

;; Printer

(define (print-term t)
  (define (print-spaces k)
    (if (= k 0) null (list (hspace k))))
  (define (print-newlines k)
    (make-list k (linebreak)))
  (define (print-subscript sub)
    (if (not sub) null (subscript (print-term sub))))
  (define (print-superscript super)
    (if (not super) null (superscript (print-term super))))
  (define (print-primes primes)
    (make-list primes (superscript (larger (element 'tt "'")))))
  (define (print-word str)
    (map (λ (c)
           (if (and (char-ci>=? c #\a)
                    (char-ci<=? c #\z))
               (italic (string c))
               (string c)))
         (string->list str)))

  (define (merge . args)
    (apply elem (flatten args)))

  (match t
    [(Term base sub super primes)
     (merge (print-term base)
            (print-subscript sub)
            (print-superscript super)
            (print-primes primes))]
    [(SPC k) (print-spaces k)]
    [(RET k) (print-newlines k)]
    [(NUM n) (format "~a" n)]
    [(LIT ℓ) ℓ]
    [(PNC p) (string p)]
    [(WRD w) (apply merge (print-word w))]
    [(list-rest ts) (map print-term (apply-mathligs ts))]))

;; Main Export

(define ($ . text-body)
  (map
   (λ (v) (if (string? v) (print-term (parse-input v)) v))
   text-body))
