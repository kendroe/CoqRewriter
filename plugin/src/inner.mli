(******************************************************************************
 *                          
 * REWRITELIB           
 *  
 * inner.mli
 *             
 * This file contains the signature for the rewrite module
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

val rewrite_in_context: (Exp.exp * Exp.exp * Exp.exp * Env.env) ->
    Exp.exp ;;
val rewrite: (Exp.exp * Env.env) -> (Exp.exp * Env.env) ;;
val rewrite_nokb: (Exp.exp * Env.env) -> (Exp.exp * Env.env) ;;
val rewrite2: Env.env -> Exp.exp -> Exp.exp list ;;

