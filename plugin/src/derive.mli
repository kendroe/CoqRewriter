(******************************************************************************
 *        
 * REWRITELIB                        
 *                                   
 * derive.mli                       
 *                               
 * This file contains the code for contextual rewriting. 
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

val set_equality_test: (Exp.exp -> Exp.exp -> bool) -> unit ;;
val equality_terms: Exp.exp -> Exp.exp -> bool ;;
val derive: Renv.env -> Exp.exp -> Exp.exp list ;;
val rule_add_derive: Renv.env -> Exp.exp -> Exp.exp list ;;




