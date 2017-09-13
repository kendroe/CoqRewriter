(******************************************************************************
 *                       
 * REWRITELIB
 *          
 * builtin.mli
 *              
 * This file contains heuristic rewriting.
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

val builtin: (Renv.env -> Exp.exp -> (Exp.exp list)) -> Renv.env -> Exp.exp -> (Exp.exp list) ;;




