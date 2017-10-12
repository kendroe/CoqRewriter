(******************************************************************************
 *
 * REWRITELIB
 *
 * crewrite.mli
 *
 * (C) 2017, Kenneth Roe
 *
 * All rights reserved--This is an incomplete work.  An appropriate license
 * will be provided when the work is complete.
 *
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *
 *****************************************************************************)

(* require "exp.sml" ;  *)
(* require "env.sml" ;  *)

(*val orient_rule: env -> Exp -> Exp*)
val create_rules: (Exp.exp -> Exp.exp list) -> Renv.env -> Exp.exp -> int -> Renv.env
val relevant_rule: Renv.env -> Exp.exp -> Exp.exp -> bool
val filter_rule_list: Renv.env -> Exp.exp -> (int list) -> (int list)
val junction_filter_rule_list: Renv.env -> Exp.exp -> (int list) -> (int list)




