#lang racket/base

(require data/applicative
         data/either
         data/monad
         megaparsack
         megaparsack/text
         racket/function
         racket/list
         racket/match
         racket/string
         (only-in scribble/base
                  elem italic hspace larger linebreak subscript superscript)
         (only-in scribble/core
                  element))

(require texmath/inferrule)

(provide $ inferrule)

;; Abstract Syntax

(struct Term (base subscript superscript primes) #:transparent)

;; Lexer

(define symbol-char/p
  (satisfy/p
   (λ (c)
     (and (char-symbolic? c)
          (not (char=? c #\^))))))

(define word-char/p
  (or/p letter/p
        digit/p
        symbol-char/p))

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

;; Parser

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

(define (parse <start> str)
  (parse-result! (parse-string <start> str)))

;; Printer

(define (merge . args)
  (apply elem (flatten args)))

(define print-token
  (match-lambda
    [(SPC k) (if (= k 0) null (list (hspace k)))]
    [(RET k) (make-list k (linebreak))]
    [(NUM n) (format "~a" n)]
    [(LIT ℓ) ℓ]
    [(PNC p) (string p)]
    [(WRD w)
     (apply merge
            (map (λ (c)
                   (if (and (char-ci>=? c #\a)
                            (char-ci<=? c #\z))
                       (italic (string c))
                       (string c)))
                 (string->list w)))]))

(define print-term
  (match-lambda
    [(Term base sub super primes)
     (merge (print-term base)
            (if (not sub) null (subscript (print-term sub)))
            (if (not super) null (superscript (print-term super)))
            (make-list primes (superscript (larger (element 'tt "'")))))]
    [(? TOK? tok) (print-token tok)]
    [(list-rest ts) (apply merge (map print-term ts))]))

;; Main Export

(define ($ . text-body)
  (merge (print-term (parse <input> (string-join text-body "")))))
