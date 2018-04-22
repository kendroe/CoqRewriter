Require Import Prelim.
Require Import Complex.
Require Import Complex_directives.
Require Import AdvancedRewrite.advancedRewrite.
Require Import Coq.Bool.BoolEq.

Check 1.

Theorem x:1+1=3.
Proof.
    arewrite.
Abort.
