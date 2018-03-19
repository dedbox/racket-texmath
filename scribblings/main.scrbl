#lang scribble/manual

@(require texmath)

@(require (for-label racket/base
                     scribble/core
                     texmath))

@title{TeXmath}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

A LaTeX-like syntax for typesetting mathematics.

@section{Syntax}

The syntax is currenty limited to whitespace-delimited sequences of
@tech{atoms} and composite @tech{terms}.

A @deftech{term} is an @tech{atom}, optionally followed by an underscore and a
subscript @tech{atom}, optionally followed by a carat and a superscript
@tech{atom}, optionally followed by a sequence of primes.

An @deftech{atom} is an contiguous sequence of characters that represents a
single, indivisible unit of information. An atom can be a number, a word, a
literal, or a sub-term.

A @deftech{number} is a contiguous sequence of decimal digits.

A @deftech{word} is a letter or symbol followed by zero or more numbers,
letters, or symbols. Latin letters are rendered in an oblique font.

A @deftech{literal} is a backslash followed by a curly brace or a sequence of
numbers, letters, and symbols. The backslash is not rendered, and the
characters following the backslash are all rendered in an upright font.
Punctuation are rendered as single-character literals.

A @deftech{sub-term} is a whitespace-delimited sequence of @tech{terms}
surrounded by curly braces.

@section{API Reference}

@defmodule[texmath #:packages ("texmath")]

@defproc[($ [text-body string?] ...+) element?]{

  Parses @racket[text-body] into an @racket[element] that, when rendered,
  resembles the output of a proper LaTeX2e renderer.

}

@defform[
  (inferrule premise ... bar maybe-rule-name conclusion)
  #:grammar
  [(maybe-rule-name (code:line)
                    (code:line rule-name))]
]{

  Renders a rule of inference with @racket[premise]s and @racket[conclusion]
  centered on and separated by a horizontal bar. If @racket[rule-name] is
  given, it is rendered to the right of the horizontal bar.

  Currently, @racket[bar] is a sequence of dashes---at least 3 but no more
  than 80.

}
