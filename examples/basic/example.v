Require Import AdvancedRewrite.advancedRewrite.

(*
(* example 1 *)
Goal forall a b, a + b + 1 = 0.
  intros. reflect_arith.
Abort.

(* example 2 *)
Goal forall a b c, a + b + 1 + c <= 1.
  intros. reflect_arith.
Abort.

(* example 3 *)
Goal forall a b c, a + b + 1 <= c.
  intros. reflect_arith.
Abort.
*)
Goal 1+2=4.
    arewrite.
Abort.

Goal 1+2=4 -> False.
    arewrite.
Abort.

Theorem x: forall q, q+1=3 -> q+1=4.
Proof.
    intros.
    arewrite.
Abort.

Theorem x: forall q, q+1=3 -> q+1=4.
Proof.
    arewrite.
Abort.

Theorem x: exists q, q+1=3 -> q+1=4.
Proof.
    arewrite.
Abort.

Theorem x: forall (x:nat) f, x = f x /\ x = f (f x).
Proof.
    arewrite.
Abort.

Theorem x: forall (x:nat) f, f (f (f x)) = f (f x) /\ x = f (f x).
Proof.
    arewrite.
Abort.

Theorem x: forall (x:nat), x=3 -> x+1=4.
Proof.
    arewrite.
Abort.

Theorem x: forall (x:nat), x=3 -> x+1=4.
Proof.
    intro x.
    arewrite.
Abort.

Theorem x: forall (x:nat), x+1=4 -> x=3.
Proof.
    intro x.
    arewrite.
Abort.

Theorem x: forall (x:nat), x+1=4 -> x=3.
Proof.
    arewrite.
Abort.

Theorem x: forall (x:nat), x+1=4 -> x=3.
Proof.
    arewrite.
Abort.

Theorem x: forall f g (x:nat) q r, (f x)+(g x)=0 -> g (x+2+1) + q + f (3+x) + r = q+r.
Proof.
    arewrite.
Abort.

Theorem x: forall (x:nat), 2*x+1=7.
Proof.
    arewrite.
Abort.

Theorem x: forall (q:nat) (x:nat) (y:nat), x+y=0 -> y+3+x=q.
Proof.
    intro q. intro x. intro y.
    arewrite.
Abort.

printExp (cons 1 nil).
