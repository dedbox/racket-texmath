#lang scribble/base

@(require texmath)

@inferrule[
      @${e_1 ↝ e_1'}
  ---------------------- "E-App1"
  @${e_1 e_2 ↝ e_1' e_2}
]

@inferrule[
  --------------------------------------- "E-AppAbs"
  @${(λx_11.e_12) v_2 ↝ [x_11↦{}v_2]e_12}
]

@inferrule[
  @${x \fresh}
  ---------------------------- "E-Def"
  @${Σ ⊢ x←v ↝ [x{}↦{}v]Σ ⊢ Ø}
]

@inferrule[
           @${Σ(x) = v}
  ---------------------
  @${Σ,Γ ⊢ x ↝ Σ,Γ ⊢ v}
]

@inferrule[
  @${\{x:T\} ∈ Γ}
  --------------- "T-Var"
  @${<Γ;x>:T}
]
