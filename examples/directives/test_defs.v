Require Import triclass.
Require Import AdvancedRewrite.advancedRewrite.

Theorem trAC_Class: AC triAdd.
Proof.
    admit.
Admitted.

Theorem trA_Class: A triAdd.
Proof.
    admit.
Admitted.

Theorem trC_Class: C triAdd.
Proof.
    admit.
Admitted.

Instance tac2 : AC_PROP triAdd :=
 {
    acProp := trAC_Class
 }.

Instance tac3 : A_PROP triAdd :=
 {
    aProp := trA_Class
 }.

Instance tac4 : C_PROP triAdd :=
 {
    cProp := trC_Class
 }.

Definition eq_fun (x:nat) (y:nat) := true.
Definition lt_fun (x:nat) (y:nat) := false.
Definition le_fun (x:nat) (y:nat) := false.

Definition eqp_fun (x:nat) (y:nat) := True.
Definition ltp_fun (x:nat) (y:nat) := False.
Definition lep_fun (x:nat) (y:nat) := False.

Theorem dd_eq: EQ eq_fun.
Proof.
    admit.
Admitted.

Theorem dd_lp: PO eq_fun lt_fun.
Proof.
    admit.
Admitted.

Theorem dd_lt: TO eq_fun lt_fun.
Proof.
    admit.
Admitted.

Theorem dd_elp: EPO eq_fun lt_fun.
Proof.
    admit.
Admitted.

Theorem dd_elt: ETO eq_fun lt_fun.
Proof.
    admit.
Admitted.

Theorem dd_eqp: EQP eqp_fun.
Proof.
    admit.
Admitted.

Theorem dd_lpp: POP eqp_fun ltp_fun.
Proof.
    admit.
Admitted.

Theorem dd_ltp: TOP eqp_fun ltp_fun.
Proof.
    admit.
Admitted.

Theorem dd_elpp: EPOP eqp_fun ltp_fun.
Proof.
    admit.
Admitted.

Theorem dd_eltp: ETOP eqp_fun ltp_fun.
Proof.
    admit.
Admitted.

Instance tac5 : EQ_PROP eq_fun :=
 {
    eqProp := dd_eq
 }.

Instance tac6 : PO_PROP eq_fun lt_fun :=
 {
    poProp := dd_lp
 }.

Instance tac7 : EPO_PROP eq_fun lt_fun :=
 {
    epoProp := dd_elp
 }.

Instance tac8 : TO_PROP eq_fun lt_fun :=
 {
    toProp := dd_lt
 }.

Instance tac9 : ETO_PROP eq_fun lt_fun :=
 {
    etoProp := dd_elt
 }.

Instance tac10 : EQP_PROP eqp_fun :=
 {
    eqpProp := dd_eqp
 }.

Instance tac11 : POP_PROP eqp_fun ltp_fun :=
 {
    popProp := dd_lpp
 }.

Instance tac12 : EPOP_PROP eqp_fun ltp_fun :=
 {
    epopProp := dd_elpp
 }.

Instance tac13 : TOP_PROP eqp_fun ltp_fun :=
 {
    topProp := dd_ltp
 }.

Instance tac14 : ETOP_PROP eqp_fun ltp_fun :=
 {
    etopProp := dd_eltp
 }.

Theorem rrp: REWRITE_RULE 1 2 True.
Proof.
    admit.
Admitted.

Instance rewriteRule : REWRITE_RULE_PROP (REWRITE_RULE 1 2 True) :=
 {
    rewriteRuleProp := rrp
 }.

Instance precLess : PREC_LESS_PROP eqp_fun ltp_fun.

Instance precEqual : PREC_EQUAL_PROP eqp_fun ltp_fun.

Theorem x: 1+1=3.
Proof.
    arewrite.
Abort.
