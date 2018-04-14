Require Export List.
Require Export AdvancedRewrite.advancedRewrite.

Definition heap := nat -> option nat.

Definition state := heap -> Prop.

Definition pointsTo x y (h : heap) := h x = Some y /\ forall x, not(x=y) -> h x=None.

Definition star (h1 : state) (h2 : state) :=
     (fun h =>
     ((forall x y n, h1 x -> h2 y -> (x n=None \/ y n=None)) /\
     (forall x n v, h1 x -> x n=Some v -> h n=Some v) /\
     (forall x n v, h2 x -> x n=Some v -> h n=Some v) /\
     (forall x y n, h1 x -> x n=None /\ h2 y -> y n=None -> h n=None))).

Definition magicWand (h1 : state) (h2 : state) h :=
     (forall x y n, h1 x -> h2 y -> x n=None -> y n=None) /\
     (forall x y n v, h1 x -> h2 y -> y n=Some v -> x n=Some v) /\
     (forall x y n v, h1 x -> h2 y -> x n=Some v -> y n=None -> h n=Some v) /\
     (forall x n v, h2 x -> x n=Some v -> h n=None) /\
     (forall x n, h1 x -> x n=None -> h n=None).

Definition empty (h : heap) := forall n, h n=None.

Fixpoint dd x := match x with | 0 => 0 | S x => S (S (dd x)) end.

Theorem starAC: AC star.
Proof.
    admit.
Admitted.

Theorem rewriteEmpty: forall x, REWRITE_RULE (star x empty) x True.
Proof.
    admit.
Admitted.

Theorem rewriteEmpty_h: forall x h, REWRITE_RULE (star x empty h) (x h) True.
Proof.
    admit.
Admitted.

Theorem wandTermElim1: forall l r x, REWRITE_RULE (magicWand (star l x) (star r x))
                                   (magicWand l r) True.
Proof.
    admit.
Admitted.

Theorem wandTermElim1_h: forall l r x h, REWRITE_RULE (magicWand (star l x) (star r x) h)
                                   (magicWand l r h) True.
Proof.
    admit.
Admitted.

Theorem wandElim1: forall l x, REWRITE_RULE (magicWand (star l x) x) l True.
Proof.
    admit.
Admitted.

Theorem wandElim1_h: forall l x h, REWRITE_RULE (magicWand (star l x) x h) (l h) True.
Proof.
    admit.
Admitted.

Theorem wandElim2: forall x, REWRITE_RULE (magicWand x x)
                               empty True.
Proof.
    admit.
Admitted.

Theorem wandElim2_h: forall x h, REWRITE_RULE (magicWand x x h)
                               (empty h) True.
Proof.
    admit.
Admitted.

Theorem wandElim3: forall x, REWRITE_RULE (magicWand x empty)
	x True.
Proof.
    admit.
Admitted.

Theorem wandElim3_h: forall x h, REWRITE_RULE (magicWand x empty h)
	(x h) True.
Proof.
    admit.
Admitted.

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


