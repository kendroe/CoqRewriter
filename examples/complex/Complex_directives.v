Require Import Prelim.
Require Import Complex.
Require Import AdvancedRewrite.advancedRewrite.

Open Scope R_scope.

Theorem CplusAC_Class: AC Cplus.
Proof.
    admit.
Admitted.

Theorem CmultAC_Class: AC Cmult.
Proof.
    admit.
Admitted.

Instance cplus_ac : AC_PROP Cplus :=
 {
    acProp := CplusAC_Class
 }.

Instance cmult_ac : AC_PROP Cmult :=
 {
    acProp := CmultAC_Class
 }.

Theorem rr_Cmod_0: REWRITE_RULE (Cmod 0) R0 True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_0 : REWRITE_RULE_PROP (REWRITE_RULE (Cmod 0) R0 True) :=
 {
    rewriteRuleProp := rr_Cmod_0
 }.

Theorem rr_Cmod_1: REWRITE_RULE (Cmod 1) R1 True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_1 : REWRITE_RULE_PROP (REWRITE_RULE (Cmod 1) R1 True) :=
 {
    rewriteRuleProp := rr_Cmod_1
 }.

Theorem rr_Cmod_opp: forall x, REWRITE_RULE (Cmod (-x)) (Cmod x) True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_opp : REWRITE_RULE_PROP (forall x, REWRITE_RULE (Cmod (-x)) (Cmod x) True) :=
 {
    rewriteRuleProp := rr_Cmod_opp
 }.

Theorem rr_Cmod_mult: forall x y, @REWRITE_RULE R (Cmod (x * y)) ((Cmod x) * (Cmod y)) True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_mult : REWRITE_RULE_PROP (forall x y, @REWRITE_RULE R (Cmod (x * y)) ((Cmod x) * (Cmod y)) True) :=
 {
    rewriteRuleProp := rr_Cmod_mult
 }.

(*Theorem rr_RtoC_plus: forall x y, REWRITE_RULE (RtoC (x + y)) ((RtoC x) + (RtoC y)) True.
Proof.
    admit.
Admitted.

Instance rule_RtoC_plus : REWRITE_RULE_PROP (forall (x:C) (y:C), REWRITE_RULE (RtoC (x + y)) ((RtoC x) + (RtoC y)) True) :=
 {
    rewriteRuleProp := rr_RtoC_plus
 }.

Theorem rr_RtoC_opp: forall x, REWRITE_RULE (RtoC (-x)) (-(RtoC x)) True.
Proof.
    admit.
Admitted.

Instance rule_RtoC_opp : REWRITE_RULE_PROP (forall x, REWRITE_RULE (RtoC (-x)) (-(RtoC x)) True) :=
 {
    rewriteRuleProp := rr_RtoC_opp
 }.

Theorem rr_RtoC_minus: forall x y, REWRITE_RULE (RtoC (x - y)) ((RtoC x) - (RtoC y)) True.
Proof.
    admit.
Admitted.

Instance rr_RtoC_minus : REWRITE_RULE_PROP (forall x y, REWRITE_RULE (RtoC (x + y)) ((RtoC x) + (RtoC y)) True) :=
 {
    rewriteRuleProp := rr_RtoC_minus
 }.*)

