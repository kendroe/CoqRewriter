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
     acProp : @AC T f
}.

Definition A {t} (f: t -> t -> t) :=
   (forall x y z, (f x (f y z))=(f (f x y) z)).

Class A_PROP {T} (f : T -> T -> T) : Prop :=
{
     aProp : @A T f
}.

Definition C {t} (f: t -> t -> t) :=
   (forall x y, (f x y)=(f y x)).

Class C_PROP {T} (f : T -> T -> T) : Prop :=
{
     cProp : @C T f
}.

Definition EQ {t} (eq: t -> t -> bool) :=
   (forall x y, (eq x y)=(eq y x)) /\
   (forall x, (eq x x)=true) /\
   (forall x y z,((eq x y)=false \/ (eq y z)=false \/ (eq x z)=true)).

Class EQ_PROP {T} (eq : T -> T -> bool) : Prop :=
{
     eqProp : @EQ T eq
}.

Definition PO {t} (eq: t -> t -> bool) (po: t -> t -> bool) :=
   (forall x y, ((po x y)=false \/ (po y x)=false)) /\
   (forall x y, (eq x y)=true -> (po x y)=false) /\
   (forall x y z,((po x y)=false \/ (po y z)=false \/ (po x z)=true)).

Class PO_PROP {T} (eq : T -> T -> bool) (po: T -> T -> bool) : Prop :=
{
     poProp : @PO T eq po
}.

Definition TO {t} (eq: t -> t -> bool) (to: t -> t -> bool) :=
   (forall x y, ((to x y)=false \/ (to y x)=false)) /\
   (forall x y, (eq x y)=true -> (to x y)=false) /\
   (forall x y, ((eq x y)=true \/ (to x y)=true \/ (to y x)=true)) /\
   (forall x y z,((to x y)=false \/ (to y z)=false \/ (to x z)=true)).

Class TO_PROP {T} (eq : T -> T -> bool) (to: T -> T -> bool) : Prop :=
{
     toProp : @TO T eq to
}.

Definition EPO {t} (eq: t -> t -> bool) (po: t -> t -> bool) :=
   (forall x y, ((po x y)=false \/ (po y x)=false)) /\
   (forall x y, (eq x y)=true -> (po x y)=true) /\
   (forall x y z,((po x y)=false \/ (po y z)=false \/ (po x z)=true)).

Class EPO_PROP {t} (eq: t -> t -> bool) (po: t -> t -> bool) : Prop :=
{
   epoProp: @EPO t eq po
}.

Definition ETO {t} (eq: t -> t -> bool) (to: t -> t -> bool) :=
   (forall x y, ((to x y)=false \/ (to y x)=false)) /\
   (forall x y, (eq x y)=true -> (to x y)=true) /\
   (forall x y, ((eq x y)=true \/ (to x y)=true \/ (to y x)=true)) /\
   (forall x y z,((to x y)=false \/ (to y z)=false \/ (to x z)=true)).

Class ETO_PROP {t} (eq: t -> t -> bool) (to: t -> t -> bool) : Prop :=
{
   etoProp: @ETO t eq to
}.

Definition EQP {t} (eq: t -> t -> Prop) :=
   (forall x y, (eq x y)=(eq y x)) /\
   (forall x, (eq x x)) /\
   (forall x y z, (eq x y) -> (eq y z) -> (eq x z)).

Class EQP_PROP {t} (eq: t -> t -> Prop) : Prop :=
{
   eqpProp: @EQP t eq
}.

Definition POP {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) :=
   (forall x y, ~(po x y) \/ ~(po y x)) /\
   (forall x y, (eq x y) -> ~(po x y)) /\
   (forall x y z,(~(po x y) \/ ~(po y z) \/ ~(po x z))).

Class POP_PROP {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) : Prop :=
{
   popProp: @POP t eq po
}.

Definition TOP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) :=
   (forall x y, (~(to x y) \/ ~(to y x))) /\
   (forall x y, (eq x y) -> ~(to x y)) /\
   (forall x y, ((eq x y) \/ (to x y) \/ (to y x))) /\
   (forall x y z,(~(to x y) \/ ~(to y z) \/ (to x z))).

Class TOP_PROP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) : Prop :=
{
   topProp: @TOP t eq to
}.

Definition EPOP {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) :=
   (forall x y, (~(po x y) \/ ~(po y x))) /\
   (forall x y, (eq x y) -> (po x y)) /\
   (forall x y z,(~(po x y) \/ ~(po y z) \/ (po x z))).

Class EPOP_PROP {t} (eq: t -> t -> Prop) (po: t -> t -> Prop) : Prop :=
{
   epopProp: @EPOP t eq po
}.

Definition ETOP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) :=
   (forall x y, (~(to x y) \/ ~(to y x))) /\
   (forall x y, (eq x y) -> (to x y)) /\
   (forall x y, ((eq x y) \/ (to x y) \/ (to y x))) /\
   (forall x y z,(~(to x y) \/ ~(to y z) \/ ~(to x z))).

Class ETOP_PROP {t} (eq: t -> t -> Prop) (to: t -> t -> Prop) : Prop :=
{
   etopProp: @ETOP t eq to
}.

Definition PREC_LESS {t1} {t2} (s1 : t1) (s2 : t2) := True.

Class PREC_LESS_PROP {t1} {t2} (l : t1) (r : t2) : Prop :=
{
}.

Definition PREC_EQUAL {t1} {t2} (s1: t1) (s2 : t2) := True.

Class PREC_EQUAL_PROP {t1} {t2} (l : t1) (r : t2) : Prop :=
{
}.

Definition REWRITE_RULE {t} (l : t) (r : t) (c: Prop) := (c -> (l=r)).

Class REWRITE_RULE_PROP (r : Prop) : Prop :=
{
    rewriteRuleProp: r
}.

