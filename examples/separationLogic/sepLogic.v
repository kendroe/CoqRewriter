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

Instance sac : AC_PROP star :=
 {
    acProp := starAC
 }.

Theorem rewriteEmpty: forall x, REWRITE_RULE (star x empty) x True.
Proof.
    admit.
Admitted.

Instance rewriteEmpty_class : REWRITE_RULE_PROP (forall x, REWRITE_RULE (star x empty) x True) :=
{
    rewriteRuleProp:= rewriteEmpty
}.

Theorem rewriteEmpty_h: forall x h, REWRITE_RULE (star x empty h) (x h) True.
Proof.
    admit.
Admitted.

Instance rewriteEmpty_h_class : REWRITE_RULE_PROP (forall x h, REWRITE_RULE (star x empty h) (x h) True) :=
 {
    rewriteRuleProp := rewriteEmpty_h
 }.

Theorem wandTermElim1: forall l r x, REWRITE_RULE (magicWand (star l x) (star r x))
                                   (magicWand l r) True.
Proof.
    admit.
Admitted.

Instance wandTermElim1_class : REWRITE_RULE_PROP (forall l r x, REWRITE_RULE (magicWand (star l x) (star r x))
                                   (magicWand l r) True) :=
 {
    rewriteRuleProp := wandTermElim1
 }.

Theorem wandTermElim1_h: forall l r x h, REWRITE_RULE (magicWand (star l x) (star r x) h)
                                   (magicWand l r h) True.
Proof.
    admit.
Admitted.

Instance wandTermElim1_h_class : REWRITE_RULE_PROP (forall l r x h, REWRITE_RULE (magicWand (star l x) (star r x) h)
                                   (magicWand l r h) True) :=
 {
    rewriteRuleProp := wandTermElim1_h
 }.

Theorem wandElim1: forall l x, REWRITE_RULE (magicWand (star l x) x) l True.
Proof.
    admit.
Admitted.

Instance wandElim1_class : REWRITE_RULE_PROP (forall l x, REWRITE_RULE (magicWand (star l x) x) l True) :=
 {
    rewriteRuleProp := wandElim1
 }.

Theorem wandElim1_h: forall l x h, REWRITE_RULE (magicWand (star l x) x h) (l h) True.
Proof.
    admit.
Admitted.

Instance wandElim1_h_class : REWRITE_RULE_PROP (forall l x h, REWRITE_RULE (magicWand (star l x) x h) (l h) True) :=
 {
    rewriteRuleProp := wandElim1_h
 }.

Theorem wandElim2: forall x, REWRITE_RULE (magicWand x x) empty True.
Proof.
    admit.
Admitted.

Instance wandElim2_class : REWRITE_RULE_PROP (forall x, REWRITE_RULE (magicWand x x) empty True) :=
 {
    rewriteRuleProp := wandElim2
 }.

Theorem wandElim2_h: forall x h, REWRITE_RULE (magicWand x x h) (empty h) True.
Proof.
    admit.
Admitted.

Instance wandElim2_h_class : REWRITE_RULE_PROP (forall x h, REWRITE_RULE (magicWand x x h) (empty h) True) :=
 {
    rewriteRuleProp := wandElim2_h
 }.

Theorem wandElim3: forall x, REWRITE_RULE (magicWand x empty)
	x True.
Proof.
    admit.
Admitted.

Instance wandElim3_class : REWRITE_RULE_PROP (forall x, REWRITE_RULE (magicWand x empty) x True) :=
 {
    rewriteRuleProp := wandElim3
 }.

Theorem wandElim3_h: forall x h, REWRITE_RULE (magicWand x empty h)
	(x h) True.
Proof.
    admit.
Admitted.

Instance wandElim3_h_class : REWRITE_RULE_PROP (forall x h, REWRITE_RULE (magicWand x empty h) (x h) True) :=
 {
    rewriteRuleProp := wandElim3_h
 }.

