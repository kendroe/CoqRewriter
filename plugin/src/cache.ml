(******************************************************************************
 *  
 * REWRITELIB
 *                            
 * cache.ml    
 *                               
 * This file contains the implementation for a cache of useful data stored
 * during rewriting.
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

(* require "bmf.sml" ;  *)
(* require "cache-s.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "list.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "ibs.sml" ; *)

(* open listimpl ; *)

open Exp ;;
(* open SUBSTimpl ; *)

module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
             | c -> c
       end

module PairsMap = Map.Make(IntPairs)

module IntSingle =
       struct
         type t = int
         let compare x y = Pervasives.compare x y
       end

module SingleMap = Map.Make(IntSingle)

module IntList =
       struct
         type t = int list
         let rec compare l1 l2 =
           match (l1,l2) with
           | ([],[]) -> 0
           | ([],x) -> 1
           | (x,[]) -> -1
           | ((i1::r1),(i2::r2)) ->
               match Pervasives.compare i1 i2 with
               | 0 -> compare r1 r2
               | c -> c
       end

module IntListMap = Map.Make(IntList)


exception NoEntry ;;

(**** Match cache ******************************************************)

let match_cache = ref (PairsMap.empty) ;;

let get_match p e =
    try (PairsMap.find (p,e) (!match_cache)) with
    Not_found -> raise NoEntry ;;

let add_match p e v =
    match_cache := PairsMap.add (p,e) v (!match_cache) ;;

(**** Unify cache ******************************************************)

let unify_cache = ref (PairsMap.empty) ;;

let get_unify p e = try (PairsMap.find (p,e) (!unify_cache)) with
                    Not_found -> raise NoEntry ;;

let add_unify p e v =
    unify_cache := PairsMap.add (p,e) v (!unify_cache) ;;

(**** Rule cache *******************************************************)

let rule_cache = ref (SingleMap.empty) ;;

let get_rules e = try (SingleMap.find e (!rule_cache)) with
                  Not_found -> raise NoEntry ;;

let add_rules e v =
    rule_cache := SingleMap.add e v (!rule_cache)

(**** uRule cache ******************************************************)

let urule_cache = ref (SingleMap.empty) ;;

let get_urules e = try (SingleMap.find e (!urule_cache)) with
                   Not_found -> raise NoEntry ;;

let add_urules e v =
    urule_cache := SingleMap.add e v (!urule_cache) ;;

(**** Rule list cache **************************************************)

let rule_list_cache = ref (IntListMap.empty) ;;

let get_context_rules e r = try IntListMap.find (e::r) (!rule_list_cache)
                            with Not_found -> raise NoEntry ;;

let add_context_rules e r v =
    rule_list_cache := IntListMap.add (e::r) v (!rule_list_cache)

(**** Unify refine cache ***********************************************)

let unify_refine_cache = ref (IntListMap.empty) ;;

let get_unify_refine e r = try IntListMap.find (e::r) (!unify_refine_cache)
                           with Not_found -> raise NoEntry ;;

let add_unify_refine e r v =
    unify_refine_cache := IntListMap.add (e::r) v (!unify_refine_cache)

(**** Relevant constants ***********************************************)

let relevant_constant_cache = ref (IntListMap.empty) ;;

let get_relevant_constants e = try IntListMap.find e (!relevant_constant_cache)
                               with Not_found -> raise NoEntry ;;

let add_relevant_constants e v =
    relevant_constant_cache := IntListMap.add e v (!relevant_constant_cache) ;;

(**** Has relevant constants *******************************************)

let has_relevant_constant_cache = ref (IntListMap.empty) ;;

let has_relevant_constants r e = try IntListMap.find (e::r) (!has_relevant_constant_cache)
                                 with Not_found -> raise NoEntry ;;

let add_has_relevant_constants r e v =
    has_relevant_constant_cache := IntListMap.add (e::r) v (!has_relevant_constant_cache)

(**** Rewrites failures ************************************************)

let failure_cache = ref (SingleMap.empty) ;;

module IntBinarySet = Set.Make(Int64) ;;

(*fun is_subset nil x = true
  | is_subset x nil = false
  | is_subset (f::r) (g::e) =
    if f=g then is_subset r e
    else if (g:int) < f then false
    else is_subset (f::r) e

fun merge_sets nil x = x
  | merge_sets x nil = x
  | merge_sets (f::r) (g::e) =
    if f=g then f::(merge_sets r e)
    else if (f:int)<g then f::g::(merge_sets r e)
    else g::f::(merge_sets r e)*)

let rec is_subset l s = match l with
  | [] -> true
  | (f::r) ->
    IntBinarySet.mem f s && is_subset r s

let is_rewrite_failure rules e =
    try is_subset (List.map (Int64.of_int) rules) (SingleMap.find e (!failure_cache))
    with Not_found -> false ;;

let add_rewrite_failure rules e =
    (failure_cache := SingleMap.add e
        (List.fold_right (fun r -> (fun s -> IntBinarySet.add (Int64.of_int r) s)) rules
            (try SingleMap.find e (!failure_cache)
             with Not_found -> IntBinarySet.empty)
            )
        (!failure_cache)) ;;

(**** Rewrites *********************************************************)

let rewrite_cache = ref (IntListMap.empty) ;;
let good_rewrite_cache = ref (IntListMap.empty) ;;

let rewrite_reads = ref 0
let good_rewrite_reads = ref 0
let rewrite_writes = ref 0

let save_good_rewrites () = (
    (*print ("Good reads = " ^ (Int.toString (!good_rewrite_reads)) ^ "\nReads = " ^ (Int.toString (!rewrite_reads)) ^ "\nWrites = " ^ (Int.toString (!rewrite_writes)) ^ "\n") ;
    good_rewrite_reads := 0 ;
    rewrite_reads := 0 ;
    rewrite_writes := 0 ;*)
    rewrite_cache := IntListMap.empty ; ())

let get_rewrite r e = (rewrite_reads := (!rewrite_reads)+1 ;
                  try (IntListMap.find (e::r) (!good_rewrite_cache))
                  with Not_found ->
                       try let x = IntListMap.find (e::r) (!rewrite_cache) in
                           (good_rewrite_cache := (IntListMap.add (e::r) x (!rewrite_cache))) ; x
                       with Not_found -> raise NoEntry)

let add_rewrite r e v =
    (*rewrite_writes := (!rewrite_writes)+1 ;*)
     rewrite_cache := IntListMap.add (e::r) v (!rewrite_cache) ;;

(**** Derived rules ****************************************************)

let derived_cache = ref (SingleMap.empty) ;;

let get_derived_rules e = try SingleMap.find e (!derived_cache)
                          with Not_found -> raise NoEntry ;;

let add_derived_rules e v =
    derived_cache := SingleMap.add e v (!derived_cache) ;;

(**** Introduced vars **************************************************)

let introduced_cache = ref (SingleMap.empty) ;;

let get_introduced_vars e = try SingleMap.find e (!introduced_cache)
                            with Not_found -> raise NoEntry ;;

let add_introduced_vars e v =
    introduced_cache := SingleMap.add e v (!introduced_cache) ;;

(**** Equal cache ******************************************************)

let equal_cache = ref (PairsMap.empty) ;;

let get_equal_terms e1 e2 = try PairsMap.find (e1,e2) (!equal_cache)
                            with Not_found -> raise NoEntry ;;

let add_equal_terms e1 e2 v =
    equal_cache := PairsMap.add (e1,e2) v (!equal_cache) ;;

(**** Mark vars cache **************************************************)

let mark_vars_cache = ref (SingleMap.empty) ;;

let get_mark_vars e = try SingleMap.find e (!mark_vars_cache)
                      with Not_found -> raise NoEntry ;;

let add_mark_vars e v =
    mark_vars_cache := SingleMap.add e v (!mark_vars_cache)

(*** Utilities *********************************************************)

let clear_cache () =
   (rule_cache := SingleMap.empty ;
    urule_cache := SingleMap.empty ;
    rule_list_cache := IntListMap.empty ;
    relevant_constant_cache := IntListMap.empty ;
    good_rewrite_cache := IntListMap.empty ;
    rewrite_cache := IntListMap.empty ;
    unify_refine_cache := IntListMap.empty
   ) ;;

let complete_clear_cache () =
   (clear_cache () ;
    mark_vars_cache := SingleMap.empty ;
    match_cache := PairsMap.empty ;
    derived_cache := SingleMap.empty ;
    introduced_cache := SingleMap.empty;
    equal_cache := PairsMap.empty
   ) ;;

