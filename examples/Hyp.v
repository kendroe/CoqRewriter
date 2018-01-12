Require Import AdvancedRewrite.advancedRewrite.
Require Import List.

Theorem test1: forall x, x=3 -> x+1=4.
Proof.
    arewrite.
Abort.

Theorem test2: forall x, x=3 -> x+1=4.
Proof.
    intro x.
    arewrite.
Abort.

Theorem test3: forall x, x=3 -> x+1=4.
Proof.
    intros.
    arewrite.
Abort.

Theorem test4: forall x, x+1=4 -> x=3.
Proof.
    intros.
    arewrite in H.
Abort.

