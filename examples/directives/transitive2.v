Require Import AdvancedRewrite.advancedRewrite.

Fixpoint beq a b :=
    match a,b with
    | 0,0 => true
    | (S x),(S y) => beq x y
    | _,_ => false
    end.

Fixpoint ble a b :=
    match a,b with
    | 0,0 => true
    | _,0 => false
    | (S x),(S y) => ble x y
    | 0,(S x) => true
    end.

Theorem trAC_Class: EQ beq.
Proof.
    admit.
Admitted.

Theorem th_lt: ETO beq ble.
Proof.
    admit.
Admitted.

Instance to1 : ETO_PROP beq ble :=
 {
    etoProp := th_lt
 }.

Theorem test1: forall x y, orb (negb (beq x y)) (negb(ble x y))=true.
Proof.
    arewrite.
Abort.

Theorem test2: forall x y, andb (negb (beq x y)) (negb(ble x y))=true.
Proof.
    arewrite.
Abort.

Theorem test3: forall x y, andb (negb (beq x y)) (ble x y)=true.
Proof.
    arewrite.
Abort.

Theorem test4: forall x y, andb (beq x y) (ble x y)=true.
Proof.
    arewrite.
Abort.

Theorem test5: forall x y, andb (beq x y) (negb (ble x y))=true.
Proof.
    arewrite.
Abort.

