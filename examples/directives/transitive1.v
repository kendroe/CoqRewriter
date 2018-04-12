Require Import AdvancedRewrite.advancedRewrite.

Fixpoint beq a b :=
    match a,b with
    | 0,0 => true
    | (S x),(S y) => beq x y
    | _,_ => false
    end.

Fixpoint blt a b :=
    match a,b with
    | _,0 => false
    | (S x),(S y) => blt x y
    | 0,(S x) => true
    end.

Theorem trAC_Class: EQ beq.
Proof.
    admit.
Admitted.

Theorem th_lt: TO beq blt.
Proof.
    admit.
Admitted.

Instance to1 : TO_PROP beq blt :=
 {
    toProp := th_lt
 }.

Theorem test1: forall x y, orb (negb (beq x y)) (negb(blt x y))=true.
Proof.
    arewrite.
Abort.

Theorem test2: forall x y, andb (negb (beq x y)) (negb(blt x y))=true.
Proof.
    arewrite.
Abort.

Theorem test3: forall x y, andb (negb (beq x y)) (blt x y)=true.
Proof.
    arewrite.
Abort.

Theorem test4: forall x y, andb (beq x y) (blt x y)=true.
Proof.
    arewrite.
Abort.

Theorem test5: forall x y, andb (beq x y) (negb (blt x y))=true.
Proof.
    arewrite.
Abort.
