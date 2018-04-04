(******************************************************************************
 *
 * REWRITELIB
 *
 * advancedRewrite.v
 *
 * This file contains Coq definitions to support the ml-plugin
 *
 * (C) 2017, Kenneth Roe
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * will be provided when the work is complete.
 *
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *
 *****************************************************************************)
Declare ML Module "theplug".

Ltac arewrite_unsafe := arewrite;[idtac | admit].

(* Definitions for the various symbol properties *)

Definition AC {t} (f: t -> t -> t) :=
   (forall x y, (f x y)=(f y x)) /\
   (forall x y z, (f x (f y z))=(f (f x y) z)).

Class AC_PROP {T} (f : T -> T -> T) : Prop :=
{
     acProp : forall a b c, (f a (f b c))=f (f a b) c /\ f a b=f b a
}.

Definition A {t} (f: t -> t -> t) :=
   (forall x y z, (f x (f y z))=(f (f x y) z)).

Class A_PROP {T} (f : T -> T -> T) : Prop :=
{
     aProp : (forall x y z, (f x (f y z))=(f (f x y) z))
}.

Definition C {t} (f: t -> t -> t) :=
   (forall x y, (f x y)=(f y x)).

Class C_PROP {T} (f : T -> T -> T) : Prop :=
{
     cProp : (forall x y z, (f x (f y z))=(f (f x y) z))
}.

Definition EQ {t} (eq: t -> t -> bool) :=
   (forall x y, (eq x y)=(eq y x)) /\
   (forall x, (eq x x)=true) /\
   (forall x y z,((eq x y)=false \/ (eq y z)=false \/ (eq x z)=true)).

Class EQ_PROP {T} (eq : T -> T -> bool) : Prop :=
{
     eqProp :
         (forall x y, (eq x y)=(eq y x)) /\
          (forall x, (eq x x)=true) /\
          (forall x y z,((eq x y)=false \/ (eq y z)=false \/ (eq x z)=true))
}.

Definition PO {t} (eq: t -> t -> bool) (po: t -> t -> bool) :=
   (forall x y, ((po x y)=false \/ (po y x)=false)) /\
   (forall x y, (eq x y)=true -> (po x y)=false) /\
   (forall x y z,((po x y)=false \/ (po y z)=false \/ (po x z)=true)).

Class PO_PROP {T} (eq : T -> T -> bool) (po: T -> T -> bool) : Prop :=
{
     poProp :
         (forall x y, ((po x y)=false \/ (po y x)=false)) /\
          (forall x y, (eq x y)=true -> (po x y)=false) /\
          (forall x y z,((po x y)=false \/ (po y z)=false \/ (po x z)=true))
}.

Definition TO {t} (eq: t -> t -> bool) (to: t -> t -> bool) :=
   (forall x y, ((to x y)=false \/ (to y x)=false)) /\
   (forall x y, (eq x y)=true -> (to x y)=false) /\
   (forall x y, ((eq x y)=true \/ (to x y)=true \/ (to y x)=true)) /\
   (forall x y z,((to x y)=false \/ (to y z)=false \/ (to x z)=true)).

Class TO_PROP {T} (eq : T -> T -> bool) (to: T -> T -> bool) : Prop :=
{
     toProp :
   (forall x y, ((to x y)=false \/ (to y x)=false)) /\
   (forall x y, (eq x y)=true -> (to x y)=false) /\
   (forall x y, ((eq x y)=true \/ (to x y)=true \/ (to y x)=true)) /\
   (forall x y z,((to x y)=false \/ (to y z)=false \/ (to x z)=true))
}.

Definition EPO {t} (eq: t -> t -> bool) (po: t -> t -> bool) :=
   (forall x y, ((po x y)=false \/ (po y x)=false)) /\
   (forall x y, (eq x y)=true -> (po x y)=true) /\
   (forall x y z,((po x y)=false \/ (po y z)=false \/ (po x z)=true)).

Class EPO_PROP {t} (eq: t -> t -> bool) (po: t -> t -> bool) : Prop :=
{
   epoProp:
       (forall x y, ((po x y)=false \/ (po y x)=false)) /\
       (forall x y, (eq x y)=true -> (po x y)=true) /\
       (forall x y z,((po x y)=false \/ (po y z)=false \/ (po x z)=true))
}.

Definition ETO {t} (eq: t -> t -> bool) (to: t -> t -> bool) :=
   (forall x y, ((to x y)=false \/ (to y x)=false)) /\
   (forall x y, (eq x y)=true -> (to x y)=true) /\
   (forall x y, ((eq x y)=true \/ (to x y)=true \/ (to y x)=true)) /\
   (forall x y z,((to x y)=false \/ (to y z)=false \/ (to x z)=true)).

Class ETO_PROP {t} (eq: t -> t -> bool) (to: t -> t -> bool) : Prop :=
{
   etoProp:
       (forall x y, ((to x y)=false \/ (to y x)=false)) /\
       (forall x y, (eq x y)=true -> (to x y)=true) /\
       (forall x y, ((eq x y)=true \/ (to x y)=true \/ (to y x)=true)) /\
       (forall x y z,((to x y)=false \/ (to y z)=false \/ (to x z)=true))
}.

Definition EQP {t} (eq: t -> t -> Prop) :=
   (forall x y, (eq x y)=(eq y x)) /\
   (forall x, (eq x x)) /\
   (forall x y z, (eq x y) -> (eq y z) -> (eq x z)).

Class EQP_PROP {t} (eq: t -> t -> Prop) : Prop :=
{
   eqpProp:
       (forall x y, (eq x y)=(eq y x)) /\
       (forall x, (eq x x)) /\
       (forall x y z, (eq x y) -> (eq y z) -> (eq x z))
}.

Definition POP {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) :=
   (forall x y, ~(po x y) \/ ~(po y x)) /\
   (forall x y, (eq x y) -> ~(po x y)) /\
   (forall x y z,(~(po x y) \/ ~(po y z) \/ ~(po x z))).

Class POP_PROP {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) : Prop :=
{
   popProp:
       (forall x y, ~(po x y) \/ ~(po y x)) /\
       (forall x y, (eq x y) -> ~(po x y)) /\
       (forall x y z,(~(po x y) \/ ~(po y z) \/ ~(po x z)))
}.

Definition TOP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) :=
   (forall x y, (~(to x y) \/ ~(to y x))) /\
   (forall x y, (eq x y) -> ~(to x y)) /\
   (forall x y, ((eq x y) \/ (to x y) \/ (to y x))) /\
   (forall x y z,(~(to x y) \/ ~(to y z) \/ (to x z))).

Class TOP_PROP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) : Prop :=
{
   topProp:
       (forall x y, (~(to x y) \/ ~(to y x))) /\
       (forall x y, (eq x y) -> ~(to x y)) /\
       (forall x y, ((eq x y) \/ (to x y) \/ (to y x))) /\
       (forall x y z,(~(to x y) \/ ~(to y z) \/ (to x z)))
}.

Definition EPOT {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) :=
   (forall x y, (~(po x y) \/ ~(po y x))) /\
   (forall x y, (eq x y) -> (po x y)) /\
   (forall x y z,(~(po x y) \/ ~(po y z) \/ (po x z))).

Class EPOT_PROP {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) : Prop :=
{
   epotProp:
       (forall x y, (~(po x y) \/ ~(po y x))) /\
       (forall x y, (eq x y) -> (po x y)) /\
       (forall x y z,(~(po x y) \/ ~(po y z) \/ (po x z)))
}.

Definition ETOP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) :=
   (forall x y, (~(to x y) \/ ~(to y x))) /\
   (forall x y, (eq x y) -> (to x y)) /\
   (forall x y, ((eq x y) \/ (to x y) \/ (to y x))) /\
   (forall x y z,(~(to x y) \/ ~(to y z) \/ ~(to x z))).

Class ETOP_PROP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) : Prop :=
{
   etopProp:
       (forall x y, (~(to x y) \/ ~(to y x))) /\
       (forall x y, (eq x y) -> (to x y)) /\
       (forall x y, ((eq x y) \/ (to x y) \/ (to y x))) /\
       (forall x y z,(~(to x y) \/ ~(to y z) \/ ~(to x z)))
}.

Definition PREC_LESS {t1} {t2} (s1: t1) (s2 : t2) := True.

Definition PREC_EQUAL {t1} {t2} (s1: t1) (s2 : t2) := True.

Definition REWRITE_RULE {t} (l : t) (r : t) (c: Prop) := (c -> (l=r)).
