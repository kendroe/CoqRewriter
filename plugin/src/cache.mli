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
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * will be provided when the work is complete.
 *           
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *  
 *****************************************************************************)
 
(* require "exp.sml" ;  *)
(* require "subst.sml" ;  *)

exception NoEntry ;;

val get_match: int -> int -> ((Rsubst.unifier * Exp.exp list) list) ;;
val add_match: int -> int -> ((Rsubst.unifier * Exp.exp list) list) -> unit ;;
val get_unify: int -> int -> ((Rsubst.unifier * Rsubst.unifier * Exp.exp list * Exp.exp list) list) ;;
val add_unify: int -> int -> ((Rsubst.unifier * Rsubst.unifier * Exp.exp list * Exp.exp list) list) -> unit ;;
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
val get_unify_refine: int -> (int list) -> Rsubst.unifier ;;
val add_unify_refine: int -> (int list) -> Rsubst.unifier -> unit ;;
val save_good_rewrites: unit -> unit ;;
val clear_cache : unit -> unit ;;
val complete_clear_cache : unit -> unit ;;

