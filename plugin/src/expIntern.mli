(******************************************************************************
 * 
 * REWRITELIB
 * 
 * expIntern.mli
 * 
 * Signature for EXP_INTERN structure
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

val intern_exp: (int -> bool) -> Exp.exp -> Exp.exp ;;
val decode_exp: Exp.exp -> Exp.exp ;;
val decode_one_exp: Exp.exp -> Exp.exp ;;
val decode_two_exp: Exp.exp -> Exp.exp ;;
val has_special_construct: Exp.exp -> bool ;;

