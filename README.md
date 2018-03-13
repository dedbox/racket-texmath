# TeXmath

Mathematical notation for Scribble docs

TeXmath is a proof-of-concept syntax extension for Racket that provides a useful subset of the LaTeX dollar sign-delimited notation for typesetting math in Scribble docs.

## Example

Logical judgments as rules of inference

```
#lang scribble/base

@(require texmath)

@inferrule[
      @${e_1 ↝ e_1'}
  ---------------------- "E-App1"
  @${e_1 e_2 ↝ e_1' e_2}
]

@inferrule[
     @${e_2 ↝ e_2'}
 ---------------------- "E-App2"
 @${v_1 e_2 ↝ v_1 e_2'}
]

@inferrule[
  --------------------------------------- "E-AppAbs"
  @${(λx_11.e_12) v_2 ↝ [x_11 ↦ v_2]e_12}
]

@inferrule[
  @${x \fresh}
  ---------------------------- "E-Def"
  @${Σ ⊢ x ← v ↝ [x ↦ v]Σ ⊢ Ø}
]

@inferrule[
           @${Σ(x) = v}
  ---------------------
  @${Σ,Γ ⊢ x ↝ Σ,Γ ⊢ v}
]

@inferrule[
  @${\{x≔T\} ∈ Γ}
  --------------- "T-Var"
   @${<Γ; x:T>}
]
```

![sample output](https://raw.githubusercontent.com/dedbox/racket-texmath/master/sample.png)
