Require Import AdvancedRewrite.advancedRewrite.

Inductive x := | Const : nat -> x | XX : x -> x -> x.

Theorem test1: XX (Const 1) (Const 2)=XX (Const 1) (Const (1+1)).
Proof.
    arewrite.
Abort.

Theorem test2: XX (Const 2) (Const 2)=XX (Const 1) (Const (1+1)).
Proof.
    arewrite.
Abort.

Theorem test3: (Const 2)=XX (Const 1) (Const (1+1)).
Proof.
    arewrite.
Abort.

Theorem test4: forall x, (Const x)=(Const 1).
Proof.
    arewrite.
Abort.

Theorem test5: forall x, (Const x)=(Const 1).
Proof.
    intros.
    arewrite.
Abort.

Theorem test6: forall x, XX (Const (x+1)) (Const 3)=XX (Const 1) (Const 3).
Proof.
    intros.
    arewrite.
Abort.

printAST (Const 4).
printAST x.

Theorem test7: forall x, x=Const 4.
Proof.
    intros.
    arewrite.
Abort.
