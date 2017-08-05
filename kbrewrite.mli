(******************************************************************************
 *                       
 * REWRITELIB
 *
 * kbrewrite.mli
 *
 * This file contains the signature for heuristic rewriting
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

(* require "env.sml" ;  *)
(* require "exp.sml" ;  *)

val possible_terms: Env.env -> Exp.exp -> ((int list) list)
val useful_subterm: Env.env -> Exp.exp -> (int list) -> bool
val kbrewrite: (Env.env -> Exp.exp -> (Exp.exp list)) -> Env.env -> Exp.exp -> (int list) -> (Exp.exp list)
val kbrewrite2: (Env.env -> Exp.exp -> (Exp.exp list)) -> Env.env -> Exp.exp -> (int list) -> (Exp.exp list)




