#lang racket/base

(require racket/function
         racket/list
         racket/match
         racket/splicing
         (only-in scribble/base
                  ~ bold elem hspace larger italic subscript superscript tabular)
         (only-in scribble/core element)
         (for-syntax racket/base
                     racket/syntax
                     scribble/base))

(provide inferrule $)

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
  (syntax-case stx
      (---
       ----
       -----
       ------
       -------
       --------
       ---------
       ----------
       -----------
       ------------
       -------------
       --------------
       ---------------
       ----------------
       -----------------
       ------------------
       -------------------
       --------------------
       ---------------------
       ----------------------
       -----------------------
       ------------------------
       -------------------------
       --------------------------
       ---------------------------
       ----------------------------
       -----------------------------
       ------------------------------
       -------------------------------
       --------------------------------
       ---------------------------------
       ----------------------------------
       -----------------------------------
       ------------------------------------
       -------------------------------------
       --------------------------------------
       ---------------------------------------
       ----------------------------------------
       -----------------------------------------
       ------------------------------------------
       -------------------------------------------
       --------------------------------------------
       ---------------------------------------------
       ----------------------------------------------
       -----------------------------------------------
       ------------------------------------------------
       -------------------------------------------------
       --------------------------------------------------
       ---------------------------------------------------
       ----------------------------------------------------
       -----------------------------------------------------
       ------------------------------------------------------
       -------------------------------------------------------
       --------------------------------------------------------
       ---------------------------------------------------------
       ----------------------------------------------------------
       -----------------------------------------------------------
       ------------------------------------------------------------
       -------------------------------------------------------------
       --------------------------------------------------------------
       ---------------------------------------------------------------
       ----------------------------------------------------------------
       -----------------------------------------------------------------
       ------------------------------------------------------------------
       -------------------------------------------------------------------
       --------------------------------------------------------------------
       ---------------------------------------------------------------------
       ----------------------------------------------------------------------
       -----------------------------------------------------------------------
       ------------------------------------------------------------------------
       -------------------------------------------------------------------------
       --------------------------------------------------------------------------
       ---------------------------------------------------------------------------
       ----------------------------------------------------------------------------
       -----------------------------------------------------------------------------
       ------------------------------------------------------------------------------
       -------------------------------------------------------------------------------
       --------------------------------------------------------------------------------)

    [(_ premise ... ---- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... --------------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ---------------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ----------------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------------------------ rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... ------------------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]
    [(_ premise ... -------------------------------------------------------------------------------- rule-name conclusion) #'(inferrule premise ... --- rule-name conclusion)]

    [(_ premise ... ---- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... --------------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ---------------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ----------------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------------------------ conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... ------------------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]
    [(_ premise ... -------------------------------------------------------------------------------- conclusion) #'(inferrule premise ... --- conclusion)]

    [(_ p premise ... --- rule-name conclusion)
     (make-named-rule #'(list p premise ...) #'conclusion #'rule-name)]

    [(_ p premise ... --- conclusion)
     (make-named-rule #'(list p premise ...) #'conclusion #'"")]

    [(_ --- rule-name conclusion)
     (make-named-rule #'(list ~) #'conclusion #'rule-name)]

    [(_ --- conclusion)
     (make-rule #'(list ~) #'conclusion)]

    [(_ conclusion) #'(elem conclusion)]))

;; Monadic Syntax

(define-syntax do
  (syntax-rules (<-)
    [(_ ((x11 ...) <- e12) e2 ...)
     (let-values ([(x11 ...) e12]) (and x11 ... (do e2 ...)))]
    [(_ (x11 <- e12) e2 ...) (let ([x11 e12]) (and x11 (do e2 ...)))]
    [(_ e1 e2 e3 ...) (and e1 (do e2 e3 ...))]
    [(_ e) e]))

;; Monadic Operations

(define (opt arg #:else [else (void)])
  (λ (in)
    (or (arg in) else)))

(define (choice . args)
  (λ (in)
    (ormap (λ (proc) (proc in)) args)))

(define (seq . args)
  (λ (in)
    (if (null? args)
        null
        (do (v <- ((car args) in))
            (vs <- ((apply seq (cdr args)) in))
            (cons v vs)))))

(define (app proc . args)
  (λ (in)
    (do (arg-vals <- ((apply seq args) in))
        (apply proc arg-vals))))

(define (app* proc make-args)
  (λ (in)
    (do (arg-vals <- (make-args in))
        (apply proc arg-vals))))

;; Characters

(define end-of-file
  (λ (in)
    (do (c <- (peek-char in))
        (eof-object? c)
        (read-char in))))

(define (next-char pred #:not [neg-pred #f])
  (λ (in)
    (do (c <- (peek-char in))
        (not (eof-object? c))
        (pred c)
        (when neg-pred
          (not (neg-pred c)))
        (read-char in))))

(define (chr c)
  (next-char (λ (c*) (eqv? c* c))))

(define newline? (curryr eqv? #\newline))
(define underscore? (curryr eqv? #\_))
(define carat? (curryr eqv? #\^))

(define whitespace-char (next-char char-whitespace? #:not newline?))
(define alphabetic-char (next-char char-alphabetic?))
(define symbolic-char (next-char char-symbolic? #:not carat?))
(define numeric-char (next-char char-numeric?))
(define punctuation-char (next-char char-punctuation? #:not underscore?))
(define word-char (choice alphabetic-char numeric-char symbolic-char))

;; Character Lists

(define (one-or-more make-char)
  (λ (in)
    (do (cs <- (let loop ([acc null])
                 (or (do (c <- (make-char in))
                         (not (eof-object? c))
                         (loop (cons c acc)))
                     (reverse acc))))
        (not (null? cs))
        cs)))

(define (zero-or-more make-char)
  (λ (in)
    (or ((one-or-more make-char) in)
        null)))

;; Lexer

(define number
  (app* (compose string->number string)
        (one-or-more numeric-char)))
(define literal
  (seq (chr #\\)
       (choice (chr #\{)
               (chr #\})
               (one-or-more word-char))))
(define word
  (app* string
        (app flatten
             (app list
                  (choice literal
                          (app (λ (c)
                                 (if (or (char=? c #\{) (char=? c #\}))
                                     c
                                     (cons #\\ c)))
                               punctuation-char)
                          (seq (choice symbolic-char alphabetic-char)
                               (zero-or-more word-char)))))))

(define token
  (choice
   (app (λ (cs) (cons 'SPACE (length cs))) (one-or-more whitespace-char))
   (app (λ (cs) (cons 'PRIME (length cs))) (one-or-more (chr #\')))
   (app (λ (cs) (cons 'NEWLINE (length cs))) (one-or-more (chr #\newline)))
   number
   word
   (chr #\_)
   (chr #\^)
   (chr #\{)
   (chr #\})
   end-of-file))

(define (lex in)
  (let loop ([acc null])
    (do (tok <- (token in))
        (if (eof-object? tok)
            (reverse (cons eof acc))
            (loop (cons tok acc))))))

;; Parsing Operators

(define (alt . args)
  (λ (toks)
    (let try-next ([procs args])
      (if (null? procs)
          (values #f toks)
          (let-values ([(v rest) ((car procs) toks)])
            (if v (values v rest) (try-next (cdr procs))))))))

(define (after arg1 arg2)
  (λ (toks)
    (define-values (v rest) (arg1 toks))
    (if v (arg2 rest) (values #f toks))))

(define (some arg)
  (λ (toks)
    (let loop ([acc null]
               [rest toks])
      (if (null? rest)
          (values (reverse acc) null)
          (let-values ([(v rest*) (arg rest)])
            (if v
                (loop (cons v acc) rest*)
                (values (reverse acc) rest)))))))

(define (many arg)
  (λ (toks)
    (let-values ([(vs rest) ((some arg) toks)])
      (if (not (null? vs))
          (values vs rest)
          (values #f toks)))))

(define (app2 proc . args)
  (λ (toks)
    (let-values ([(vs rest) ((apply seq2 args) toks)])
      (if vs
          (let ([v* (apply proc vs)])
            (if v* (values v* rest) (values #f toks)))
          (values #f toks)))))

(define (opt2 arg #:else [else (void)])
  (λ (toks)
    (let-values ([(v rest) (arg toks)])
      (if v
          (values v rest)
          (values else toks)))))

(define (seq2 . args)
  (λ (toks)
    (let loop ([acc null]
               [procs args]
               [rest toks])
      (cond [(null? procs) (values (reverse acc) rest)]
            [(null? rest) (values #f toks)]
            [else
             (let-values ([(v rest*) ((car procs) rest)])
               (if v
                   (loop (cons v acc) (cdr procs) rest*)
                   (values #f toks)))]))))

;; Parser Tokens

(define-syntax-rule (tok pat exp)
  (match-lambda [`pat exp] [_ #f]))

(define (next-token proc)
  (λ (toks)
    (if (null? toks)
        (values #f null)
        (let ([v (proc (car toks))])
          (if v (values v (cdr toks)) (values #f toks))))))

(define (pred->next-token pred)
  (next-token (λ (v) (and (pred v) v))))

(define string-token (pred->next-token string?))
(define number-token (pred->next-token number?))

;; Parser Rules

(define spaces (next-token (tok (SPACE . ,k) k)))
(define newlines (next-token (tok (NEWLINE . ,k) k)))
(define primes (next-token (tok (PRIME . ,k) k)))
(define underscore (next-token (tok #\_ '_)))
(define carat (next-token (tok #\^ '^)))
(define left-brace (next-token (tok #\{ #t)))
(define right-brace (next-token (tok #\} #t)))

(define-values
  (atom term)
  (letrec
      ([atom (λ (in)
               ((alt string-token
                     number-token
                     (app2 (λ (vs)
                             (if (null? vs)
                                 (cons (caddr vs) (list 0 "" (void) (void) 0))
                                 (cons (caddr vs) (cadr vs))))
                           (seq2 left-brace
                                 terms
                                 (app2 length (some (alt spaces newlines)))
                                 right-brace))) in))]
       [term (λ (in)
               ((app2 cons
                      (app2 length (some (alt spaces newlines)))
                      (alt (seq2 string-token
                                 (opt2 (after underscore atom))
                                 (opt2 (after carat atom))
                                 (opt2 primes #:else 0))
                           number-token)) in))])
    (values atom term)))

(define terms (many term))

;; Parser

(define (parse in)
  (do (toks <- (lex in))
      ((vs rest) <- (terms toks))
      ((_ rest*) <- ((some (alt spaces newlines)) rest))
      (and (equal? rest* (list eof)) vs)))

;; Special Words

(define special-words
  (hash "=>" "⇒"
        "==>" "⇒"
        "{" ""
        "}" ""
        ;; "\\sqrt" (λ (arg) (cons "√" arg))
        ))

(define (mathlig str remap)
  (hash-set! special-words str remap))

(define (lig s)
  (hash-ref special-words s s))

;; Printer

(define (print-spaces n)
  (if (= n 0) null (list (hspace n))))

(define print-atom
  (match-lambda
    [(? string? s)
     (let ([s* (lig s)])
       (if (equal? s* "")
           ""
           (let ([c (string-ref s* 0)])
             (cond
              [(and (char-ci>=? c #\a) (char-ci<=? c #\z)) (italic s*)]
              [(eqv? c #\\) (substring s* 1)]
              [else s*]))))]
    [(? number? n) (format "~a" n)]
    [(cons (? number? n) vs)
     (apply elem (flatten (list (map print-term vs) (print-spaces n))))]
    [v (raise `(UNPRINTABLE ,v))]))

(define (print-subscript sub)
  (if (void? sub) null (subscript (print-atom sub))))

(define (print-superscript super)
  (if (void? super) null (superscript (print-atom super))))

(define prime (superscript (larger (element 'tt "'"))))

(define print-term
  (match-lambda
    [(cons n (list base sub super ps))
     (writeln `(TERM ,(list n base sub super ps)))
     (apply elem (flatten (list (print-spaces n)
                                (print-atom base)
                                (print-subscript sub)
                                (print-superscript super)
                                (make-list ps prime))))]
    [(cons n v)
     (apply elem (flatten (list (print-spaces n)
                                (print-atom v))))]))

(define ($ text-body)
  (or (do (ts <- (parse (open-input-string text-body)))
          (apply elem (map print-term ts)))
      (raise `(TeXmath-SYNTAX-ERROR ,text-body))))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
    "lex"
    (check equal? (lex (open-input-string "ab12cd34")) `("ab12cd34" ,eof))
    (check equal? (lex (open-input-string "12ab34cd")) `(12 "ab34cd" ,eof))
    (check equal?
           (lex (open-input-string "\n  a' 1b_i^2'''"))
           `((NEWLINE . 1) (SPACE . 2)
             "a" (PRIME . 1)
             (SPACE . 1)
             1
             "b" #\_ "i" #\^ 2 (PRIME . 3)
             ,eof)))

  (test-case
    "parse"
    (check equal?
           (parse (open-input-string "\n  a_3' 1b_i^2'''"))
           `((2 "a" 3 ,(void) 1)
             (1 . 1)
             (0 "b" "i" 2 3)))))
