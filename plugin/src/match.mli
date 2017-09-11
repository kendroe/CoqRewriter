(******************************************************************************
 *  
 * REWRITELIB
 *                            
 * match.mli
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
(* require "subst.sml" ;  *)
(* require "env.sml" ;  *)

val thematch: Env.env -> Exp.exp -> Exp.exp -> ((Rsubst.unifier * Exp.exp list) list) ;;
val unify: Env.env -> Exp.exp -> Exp.exp -> ((Rsubst.unifier * Rsubst.unifier * Exp.exp list * Exp.exp list) list) ;;
val equal: Env.env -> Exp.exp -> Exp.exp -> bool ;;
val equal_smaller: Env.env -> Exp.exp -> Exp.exp -> bool ;;
val much_smaller: Env.env -> Exp.exp -> Exp.exp -> bool ;;
val remove_equal_dups: Env.env -> Exp.exp list -> Exp.exp list ;;





