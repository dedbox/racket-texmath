#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-tokens words (SYM PUNC OPER WORD))
(define-empty-tokens delim
  (SPACE DOLLAR UNDERSCORE CARAT PRIME LEFT-BRACE RIGHT-BRACE EOF))

(define texmath-lexer
  (lexer-src-pos
   [whitespace (return-without-pos (texmath-lexer input-port))]
   [#\$ 'DOLLAR]
   [#\_ 'UNDERSCORE]
   [#\^ 'CARAT]
   [#\' 'PRIME]
   [#\{ 'LEFT-BRACE]
   [#\} 'RIGHT-BRACE]
   [lit (token-SYM (substring lexeme 1))]
   [sym (token-SYM lexeme)]
   [word (token-WORD lexeme)]
   [(eof) 'EOF]))

(define-lex-abbrevs
  [lit (:: #\\ (:or #\\ #\{ #\} word))]
  [word (:: (:or (:/ "A" "Z")
                 (:/ "a" "z"))
            (:* (:/ "0" "9")
                (:/ "A" "Z")
                (:/ "a" "z")))]
  [sym (:or (:+ (:/ "0" "9"))
            (char-set "!()=+-*/,.:;<>?@[]|ΣΓλØ≔∈↦̱←↝⊢"))])
