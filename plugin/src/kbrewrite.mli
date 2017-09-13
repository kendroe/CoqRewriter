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

val possible_terms: Renv.env -> Exp.exp -> ((int list) list)
val useful_subterm: Renv.env -> Exp.exp -> (int list) -> bool
val kbrewrite: (Renv.env -> Exp.exp -> (Exp.exp list)) -> Renv.env -> Exp.exp -> (int list) -> (Exp.exp list)
val kbrewrite2: (Renv.env -> Exp.exp -> (Exp.exp list)) -> Renv.env -> Exp.exp -> (int list) -> (Exp.exp list)




