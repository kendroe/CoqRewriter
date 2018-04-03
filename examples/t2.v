Require Import AdvancedRewrite.advancedRewrite.
Require Import triclass.

Theorem trAC_Class: forall a b c, (triAdd a (triAdd b c))=triAdd (triAdd a b) c /\ triAdd a b=triAdd b a.
Proof.
    admit.
Admitted.

Instance tac2 : AC_PROP triAdd :=
 {
    acProp := trAC_Class
 }.

Theorem x: 1+1=3.
Proof.
    arewrite.
Abort.
