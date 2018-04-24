Require Import Prelim.
Require Import Complex.
Require Import AdvancedRewrite.advancedRewrite.

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

Theorem rr_Cmod_mult: forall x y, REWRITE_RULE (Cmod (x * y)) ((Cmod x) * (Cmod y)) True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_mult : REWRITE_RULE_PROP (forall x y, REWRITE_RULE (Cmod (x * y)) ((Cmod x) * (Cmod y)) True) :=
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

Theorem rr_Cplus_0_r: forall r, @REWRITE_RULE R (r+0) (r) True.
Proof.
    admit.
Admitted.

Instance rule_Cplus_0_r : REWRITE_RULE_PROP (forall r, @REWRITE_RULE R (r+0) (r) True) :=
 {
    rewriteRuleProp := rr_Cplus_0_r
 }.

Theorem rr_Cplus_opp_r: forall r, @REWRITE_RULE R (r+(-r)) 0 True.
Proof.
    admit.
Admitted.

Instance rule_Cplus_opp_r : REWRITE_RULE_PROP (forall r, @REWRITE_RULE R (r+(-r)) 0 True) :=
 {
    rewriteRuleProp := rr_Cplus_opp_r
 }.

Theorem rr_Copp_plus_distr: forall z1 z2, REWRITE_RULE (-(z1+z2)) ((-z1)+(-z2)) True.
Proof.
    admit.
Admitted.

Instance rule_Copp_plus_distr : REWRITE_RULE_PROP (forall z1 z2, REWRITE_RULE (-(z1+z2)) ((-z1)+(-z2)) True) :=
 {
    rewriteRuleProp := rr_Copp_plus_distr
 }.

Theorem rr_Copp_minus_distr: forall z1 z2, REWRITE_RULE (-(z1-z2)) (z2-z1) True.
Proof.
    admit.
Admitted.

Instance rule_Copp_minus_distr : REWRITE_RULE_PROP (forall z1 z2, REWRITE_RULE (-(z1-z2)) (z2-z1) True) :=
 {
    rewriteRuleProp := rr_Copp_minus_distr
 }.

Theorem rr_Cmult_0_r: forall r, REWRITE_RULE (r*0) (0) True.
Proof.
    admit.
Admitted.

Instance rule_Cmult_0_r : REWRITE_RULE_PROP (forall r, REWRITE_RULE (r*0) (0) True) :=
 {
    rewriteRuleProp := rr_Cmult_0_r
 }.

Theorem rr_Cmult_1_r: forall r, REWRITE_RULE (r*1) (r) True.
Proof.
    admit.
Admitted.

Instance rule_Cmult_1_r : REWRITE_RULE_PROP (forall r, REWRITE_RULE (r*1) (r) True) :=
 {
    rewriteRuleProp := rr_Cmult_1_r
 }.

Theorem rr_Cinv_r: forall r, REWRITE_RULE (r* /r) (1) (r<0).
Proof.
    admit.
Admitted.

Instance rule_Cinv_r : REWRITE_RULE_PROP (forall r, REWRITE_RULE (r* /r) (1) (r<0)) :=
 {
    rewriteRuleProp := rr_Cinv_r
 }.

Theorem rr_Cmult_plus_distr: forall x y z, REWRITE_RULE (x * (y + z)) (x * y + x * z) True.
Proof.
    admit.
Admitted.

Instance rule_Cmult_plus_distr : REWRITE_RULE_PROP (forall x y z, REWRITE_RULE (x * (y + z)) (x * y + x * z) True) :=
 {
    rewriteRuleProp := rr_Cmult_plus_distr
 }.

Theorem rr_Copp_0: REWRITE_RULE (Copp 0) (0) True.
Proof.
    admit.
Admitted.

Instance rule_Copp_0 : REWRITE_RULE_PROP (REWRITE_RULE (Copp 0) (0) True) :=
 {
    rewriteRuleProp := rr_Copp_0
 }.

Theorem rr_Cmod_m1: REWRITE_RULE (Cmod (Copp 1)) (1) True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_m1 : REWRITE_RULE_PROP (REWRITE_RULE (Cmod (Copp 1)) (1) True) :=
 {
    rewriteRuleProp := rr_Cmod_m1
 }.

Theorem rr_Cmod_eq_0: forall x, REWRITE_RULE (Cmod x=0) (x=0) True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_eq_0 : REWRITE_RULE_PROP (forall x, REWRITE_RULE (Cmod x=0) (x=0) True) :=
 {
    rewriteRuleProp := rr_Cmod_eq_0
 }.

Theorem rr_Cmod_ge_0: forall x, REWRITE_RULE (0 <= Cmod x) True True.
Proof.
    admit.
Admitted.

Instance rule_Cmod_ge_0 : REWRITE_RULE_PROP (forall x, REWRITE_RULE (0 <= Cmod x) True True) :=
 {
    rewriteRuleProp := rr_Cmod_ge_0
 }.

Theorem rr_Cmod_gt_0: forall x, REWRITE_RULE (0 < Cmod x) True (x<>0).
Proof.
    admit.
Admitted.

Instance rule_Cmod_gt_0 : REWRITE_RULE_PROP (forall x, REWRITE_RULE (0 < Cmod x) True (x<>0)) :=
 {
    rewriteRuleProp := rr_Cmod_gt_0
 }.

Theorem rr_Cmod_inv: forall x, REWRITE_RULE (Cmod (/x)) (Rinv (Cmod x)) (x<>0).
Proof.
    admit.
Admitted.

Instance rule_Cmod_inv : REWRITE_RULE_PROP (forall x, REWRITE_RULE (Cmod (/x)) (Rinv (Cmod x)) (x<>0)) :=
 {
    rewriteRuleProp := rr_Cmod_inv
 }.

Theorem rr_Cmod_div: forall x y, REWRITE_RULE (Cmod (x / y)) (Rdiv (Cmod x) (Cmod y)) (y<>0).
Proof.
    admit.
Admitted.

Instance rule_Cmod_div : REWRITE_RULE_PROP (forall x y, REWRITE_RULE (Cmod (x / y)) (Rdiv (Cmod x) (Cmod y)) (y<>0)) :=
 {
    rewriteRuleProp := rr_Cmod_div
 }.

Theorem rr_Cmult_neq_0: forall x y, REWRITE_RULE (x * y <> 0) (True) (x <> 0 /\ y <> 0).
Proof.
    admit.
Admitted.

Instance rule_Cmult_neq_0 : REWRITE_RULE_PROP (forall x y, REWRITE_RULE (x * y <> 0) (True) (x <> 0 /\ y <> 0)) :=
 {
    rewriteRuleProp := rr_Cmult_neq_0
 }.

Theorem rr_Cminus_eq_contra: forall r1 r2, REWRITE_RULE (r1 - r2 <> 0) (r1 <> r2) True.
Proof.
    admit.
Admitted.

Instance rule_Cminus_eq_contra : REWRITE_RULE_PROP (forall r1 r2, REWRITE_RULE (r1 - r2 <> 0) (r1 <> r2) True) :=
 {
    rewriteRuleProp := rr_Cminus_eq_contra
 }.

(*Theorem rr_RtoC_pow: forall r n, REWRITE_RULE ((RtoC r)^n) (RtoC (r^n)) True.
Proof.
    admit.
Admitted.

Instance rule_RtoC_pow : REWRITE_RULE_PROP (forall r n, REWRITE_RULE ((RtoC r)^n) (RtoC (r^n)) True) :=
 {
    rewriteRuleProp := rr_RtoC_pow
 }.*)

Theorem rr_Cplus_opp: forall c, REWRITE_RULE ((-c) + c) (0) True.
Proof.
    admit.
Admitted.

Instance rule_Cplus_opp : REWRITE_RULE_PROP (forall c, REWRITE_RULE ((-c) + c) (0) True) :=
 {
    rewriteRuleProp := rr_Cplus_opp
 }.

Theorem rr_Copp_involutive: forall c, REWRITE_RULE (- - c) (c) True.
Proof.
    admit.
Admitted.

Instance rule_Copp_involutive : REWRITE_RULE_PROP (forall c, REWRITE_RULE (- - c) (c) True) :=
 {
    rewriteRuleProp := rr_Copp_involutive
 }.

Theorem rr_Cfst: forall (c : Complex.C), REWRITE_RULE ((fst c)<>0) (c<>0) True.
Proof.
    admit.
Admitted.

Instance rule_Cfst : REWRITE_RULE_PROP (forall (c:Complex.C), REWRITE_RULE (fst c<>0) (c<>0) True) :=
 {
    rewriteRuleProp := rr_Cfst
 }.

Theorem rr_Csnd: forall (c : Complex.C), REWRITE_RULE ((snd c)<>0) (c<>0) True.
Proof.
    admit.
Admitted.

Instance rule_Csnd : REWRITE_RULE_PROP (forall (c:Complex.C), REWRITE_RULE (snd c<>0) (c<>0) True) :=
 {
    rewriteRuleProp := rr_Csnd
 }.

Theorem rr_Cinv_mult_distr: forall c1 c2, REWRITE_RULE (/ (c1 * c2)) ((/ c1) * (/ c2)) True.
Proof.
    admit.
Admitted.

Instance rule_Cinv_mult_distr : REWRITE_RULE_PROP (forall c1 c2, REWRITE_RULE (/ (c1 * c2)) ((/ c1) * (/ c2)) True) :=
 {
    rewriteRuleProp := rr_Cinv_mult_distr
 }.


