(******************************************************************************
 *
 * REWRITELIB
 *
 * subst.mli
 * 
 * Substitution stuff
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

type unifier ;;

(*
 * Constructor functions
 *)
val empty: unifier ;;
val addPair: unifier -> int -> Exp.exp -> unifier ;;
val addNormals: unifier -> unifier ;;

(*
 * Retrieval
 *)
exception NoUnifier ;;

val dom: unifier -> int list ;;
val apply: unifier -> int -> Exp.exp ;;
val subst: unifier -> Exp.exp -> Exp.exp ;;
val select_subs: unifier -> (int list) -> unifier ;;
val remove_subs: unifier -> (int list) -> unifier ;;
val merge_subs: unifier -> unifier -> unifier ;;
val compose: unifier -> unifier -> unifier ;;





