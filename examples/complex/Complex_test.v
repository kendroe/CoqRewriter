Require Import Prelim.
Require Import Complex.
Require Import Complex_directives.
Require Import AdvancedRewrite.advancedRewrite.
Open Scope R_scope.

Theorem test1:1+1=3.
Proof.
    arewrite.
Abort.

Theorem test2: forall a b c d, Complex.Cmult (Complex.Cplus a b) (Complex.Cplus c d)=0.
Proof.
    arewrite.
Abort.
