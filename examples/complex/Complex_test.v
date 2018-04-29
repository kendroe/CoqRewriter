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

Theorem test2: forall a c, Complex.Cmult (Complex.Cplus a 0) (Complex.Cplus c 0)=0.
Proof.
    arewrite.
Abort.

Theorem test3: (Complex.Cmod (-(1))) =1.
Proof.
    arewrite.

    reflexivity.
Abort.

Theorem test4: (Complex.Cmod (-1)) =1.
Proof.
    arewrite.
Abort.

Theorem test5: forall a b c, (/(a*b*c))%R =1.
Proof.
    arewrite.
Abort.

Theorem test6: forall a b c, (/(a*b*c))%C =1.
Proof.
    arewrite.
Abort.

Theorem test7: forall x, (Cmod ((x+(-x))%C))=0.
Proof.
    arewrite.
    reflexivity.
Abort.

Theorem test8: forall x, (Cmod ((x+(-x))))=0.
Proof.
    arewrite.
    reflexivity.
Abort.

Theorem test9: forall x, (Cmod (x*0))=0.
Proof.
    arewrite.
    reflexivity.
Abort.

Theorem test10: forall x, (Cmod ((x*0)%C))=0.
Proof.
    arewrite.
    reflexivity.
Abort.
