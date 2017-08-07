(******************************************************************************
 *  
 * REWRITELIB
 *                            
 * cache.mli
 *                               
 * Signature for CACHE structure
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

exception NoEntry ;;

val get_match: int -> int -> ((Subst.unifier * Exp.exp list) list) ;;
val add_match: int -> int -> ((Subst.unifier * Exp.exp list) list) -> unit ;;
val get_unify: int -> int -> ((Subst.unifier * Subst.unifier * Exp.exp list * Exp.exp list) list) ;;
val add_unify: int -> int -> ((Subst.unifier * Subst.unifier * Exp.exp list * Exp.exp list) list) -> unit ;;
val get_rules: int -> (int list) ;;
val add_rules: int -> (int list) -> unit ;;
val get_urules: int -> (int list) ;;
val add_urules: int -> (int list) -> unit ;;
val get_context_rules : int -> (int list) -> (int list) ;;
val add_context_rules : int -> (int list) -> (int list) -> unit ;;
val get_relevant_constants: (int list) -> (int list) ;;
val add_relevant_constants: (int list) -> (int list) -> unit ;;
val has_relevant_constants: (int list) -> int -> bool ;;
val add_has_relevant_constants: (int list) -> int -> bool -> unit ;;
val get_rewrite: (int list) -> int -> int ;;
val add_rewrite: (int list) -> int -> int -> unit ;;
val is_rewrite_failure: (int list) -> int -> bool ;;
val add_rewrite_failure: (int list) -> int -> unit ;;
val get_derived_rules: int -> (int list) ;;
val add_derived_rules: int -> (int list) -> unit ;;
val get_equal_terms: int -> int -> bool ;;
val add_equal_terms: int -> int -> bool -> unit ;;
val get_mark_vars: int -> int ;;
val add_mark_vars: int -> int -> unit ;;
val get_introduced_vars: int -> (int list) ;;
val add_introduced_vars: int -> (int list) -> unit ;;
val get_unify_refine: int -> (int list) -> Subst.unifier ;;
val add_unify_refine: int -> (int list) -> Subst.unifier -> unit ;;
val save_good_rewrites: unit -> unit ;;
val clear_cache : unit -> unit ;;
val complete_clear_cache : unit -> unit ;;

