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
 * All rights reserved--This is an incomplete work.  An appropriate license
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
    relevant_constant_cache := IntListMap.insert e v (!relevant_constant_cache) ;;

(**** Has relevant constants *******************************************)

val has_relevant_constant_cache = ref (IntListDict.empty : bool IntListDict.map)

fun has_relevant_constants r e = case (IntListDict.find (!has_relevant_constant_cache,e::r))
                    of SOME x => x
                     | NONE   => raise NoEntry

fun add_has_relevant_constants r e v =
    has_relevant_constant_cache := IntListDict.insert (!has_relevant_constant_cache,e::r,v)

(**** Rewrites failures ************************************************)

let failure_cache = ref (SingleMap.empty)

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

fun is_subset nil s = true
  | is_subset (f::r) s =
    IntBinarySet.member (s,f) andalso is_subset r s

fun is_rewrite_failure rules e =
    case (IntDict.find (!failure_cache,e))
      of SOME x => is_subset rules x
       | NONE   => false

fun add_rewrite_failure rules e =
    (failure_cache := IntDict.insert (!failure_cache,e,
        IntBinarySet.addList
            (case (IntDict.find ((!failure_cache),e))
               of SOME x => x
                | NONE   => IntBinarySet.empty,rules)))

(**** Rewrites *********************************************************)

val rewrite_cache = ref (IntListDict.empty : int IntListDict.map)
val good_rewrite_cache = ref (IntListDict.empty : int IntListDict.map)

val rewrite_reads = ref 0
val good_rewrite_reads = ref 0
val rewrite_writes = ref 0

fun save_good_rewrites () = (
    (*print ("Good reads = " ^ (Int.toString (!good_rewrite_reads)) ^ "\nReads = " ^ (Int.toString (!rewrite_reads)) ^ "\nWrites = " ^ (Int.toString (!rewrite_writes)) ^ "\n") ;
    good_rewrite_reads := 0 ;
    rewrite_reads := 0 ;
    rewrite_writes := 0 ;*)
    rewrite_cache := IntListDict.empty ; ())

fun get_rewrite r e = (rewrite_reads := (!rewrite_reads)+1 ;
                  case (IntListDict.find (!good_rewrite_cache,e::r))
                    of SOME x => ((*good_rewrite_reads := (!good_rewrite_reads+1) ;*) x)
             | NONE   => case (IntListDict.find (!rewrite_cache,e::r))
                                           of SOME x => ((*good_rewrite_reads := (!good_rewrite_reads+1) ;*)
                                                 good_rewrite_cache := IntListDict.insert (!rewrite_cache,e::r,x) ;
                                                 x)
                                    | NONE   => raise NoEntry)

fun add_rewrite r e v =
    ((*rewrite_writes := (!rewrite_writes)+1 ;*)
     rewrite_cache := IntListDict.insert (!rewrite_cache,e::r,v))

(**** Derived rules ****************************************************)

val derived_cache = ref (IntDict.empty : int list IntDict.map)

fun get_derived_rules e = case (IntDict.find (!derived_cache,e))
                    of SOME x => x
                     | NONE   => raise NoEntry

fun add_derived_rules e v =
    derived_cache := IntDict.insert (!derived_cache,e,v)

(**** Introduced vars **************************************************)

val introduced_cache = ref (IntDict.empty : int list IntDict.map)

fun get_introduced_vars e = case (IntDict.find (!introduced_cache,e))
                    of SOME x => x
                     | NONE   => raise NoEntry

fun add_introduced_vars e v =
    introduced_cache := IntDict.insert (!introduced_cache,e,v)

(**** Equal cache ******************************************************)

val equal_cache = ref (DoubleIntDict.empty : bool DoubleIntDict.map)

fun get_equal_terms e1 e2 = case (DoubleIntDict.find (!equal_cache,(e1,e2)))
                    of SOME x => x
                     | NONE   => raise NoEntry

fun add_equal_terms e1 e2 v =
    equal_cache := DoubleIntDict.insert (!equal_cache,(e1,e2),v)

(**** Mark vars cache **************************************************)

val mark_vars_cache = ref (IntDict.empty : int IntDict.map)

fun get_mark_vars e = case (IntDict.find (!mark_vars_cache,e))
                    of SOME x => x
                     | NONE   => raise NoEntry

fun add_mark_vars e v =
    mark_vars_cache := IntDict.insert (!mark_vars_cache,e,v)

(*** Utilities *********************************************************)

fun clear_cache () =
   (rule_cache := IntDict.empty ;
    urule_cache := IntDict.empty ;
    rule_list_cache := IntListDict.empty ;
    relevant_constant_cache := IntListDict.empty ;
    good_rewrite_cache := IntListDict.empty ;
    rewrite_cache := IntListDict.empty ;
    unify_refine_cache := IntListDict.empty
   )

fun complete_clear_cache () =
   (clear_cache () ;
    mark_vars_cache := IntDict.empty ;
    match_cache := DoubleIntDict.empty ;
    derived_cache := IntDict.empty ;
    introduced_cache := IntDict.empty;
    equal_cache := DoubleIntDict.empty
   )

