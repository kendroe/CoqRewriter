Require Export List.
Require Export AdvancedRewrite.advancedRewrite.
Require Export sepLogic.

Theorem test1: forall a h, (star a empty) h.
Proof.
    arewrite.
Admitted.

Theorem test2: forall a b c h, (magicWand (star a (star b c)) b) h.
Proof.
    arewrite.
Admitted.

Theorem test3: forall a b c h, (magicWand (star a (star b c)) (star a b)) h.
Proof.
    arewrite.
Admitted.

Theorem test4: forall a b h, (magicWand (star b a) a) h.
Proof.
    arewrite.
Admitted.

Theorem test5: forall a h, (magicWand a a) h.
Proof.
    arewrite.
Admitted.

Theorem test6: forall a h, (magicWand (star a empty) a) h.
Proof.
    arewrite.
Admitted.

Theorem test7: forall a b h, (star (star a empty) b) h.
Proof.
    arewrite.
Admitted.


