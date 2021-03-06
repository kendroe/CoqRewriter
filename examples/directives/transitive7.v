Require Import AdvancedRewrite.advancedRewrite.

Fixpoint beq a b :=
    match a,b with
    | 0,0 => True
    | (S x),(S y) => beq x y
    | _,_ => False
    end.

Fixpoint blt a b :=
    match a,b with
    | _,0 => False
    | (S x),(S y) => blt x y
    | 0,(S x) => True
    end.

Theorem th_lt: POP beq blt.
Proof.
    admit.
Admitted.

Instance po1 : POP_PROP beq blt :=
 {
    popProp := th_lt
 }.

Theorem test1: forall x y, ~(beq x y) \/ ~(blt x y).
Proof.
    arewrite.
Abort.

Theorem test2: forall x y, ~(beq x y) /\ ~(blt x y).
Proof.
    arewrite.
Abort.

Theorem test3: forall x y, ~(beq x y) /\ ~(blt x y).
Proof.
    arewrite.
Abort.

Theorem test4: forall x y, (beq x y) /\ (blt x y).
Proof.
    arewrite.
Abort.

Theorem test5: forall x y, (beq x y) /\ ~(blt x y).
Proof.
    arewrite.
Abort.
