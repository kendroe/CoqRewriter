(******************************************************************************
 *
 * REWRITELIB
 *
 * expIntern.ml
 *
 * This file contains the implementation for expression interning
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
 
(* require "list.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "bmf.sml" ;  *)
(* require "expint-s.sml" ;  *)
(* require "basis.__array" ;  *)
(* require "lm.sml" ; *)

module IntSingle =
       struct
         type t = int
         let compare x y = Pervasives.compare x y
       end

module SingleMap = Map.Make(IntSingle)

module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
             | c -> c
       end

module PairsMap = Map.Make(IntPairs)

module Chars =
       struct
         type t = char
         let compare c0 c1 =
           Pervasives.compare (int_of_char c0) (int_of_char c1)
       end

module CharMap = Map.Make(Chars)

module Strings =
       struct
         type t = string
         let compare s0 s1 =
           Pervasives.compare s0 s1
       end

module StringMap = Map.Make(Strings)

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

module IntTriples =
       struct
         type t = int * int * int
         let compare (x0,y0,z0) (x1,y1,z1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
             | c -> c
       end

module TriplesMap = Map.Make(IntTriples)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

(* open listimpl ; *)
(* open Exp ; *)
(* open INTERNimpl ; *)
(* open Array ; *)

(* structure DoubleIntDict = BinaryMapFn(DOUBLE_INT_KEY) ; *)
(* structure TripleIntDict = BinaryMapFn(TRIPLE_INT_KEY) ; *)
(* structure IntListDict = BinaryMapFn(INT_LIST_KEY) ; *)
(* structure IntDict = BinaryMapFn(INT_KEY) ; *)
(* structure StringDict = BinaryMapFn(STRING_KEY) ; *)
(* structure CharDict = BinaryMapFn(CHAR_KEY) ; *)

open Exp;;

let intern_count = ref 1 ;;
let array_size = ref 100 ;;

let decode_array = ref (Array.make 100 (ref NOEXP)) ;;
let rec_decode_array = ref (Array.make 100 (ref NOEXP)) ;;
let parent_array : (int list) IntListMap.t ref array ref = ref (Array.make 100 (ref (IntListMap.empty))) ;;
let special_construct_array = ref (Array.make 100 (ref (false : bool))) ;;

let nil_ac_symbol_encode : int SingleMap.t ref = ref (SingleMap.empty) ;;
let nil_symbol_encode : int SingleMap.t ref = ref (SingleMap.empty) ;;
let var_encode : int SingleMap.t ref = ref (SingleMap.empty) ;;
let marked_var_encode : int SingleMap.t ref = ref (SingleMap.empty) ;;
let string_encode : int StringMap.t ref = ref (StringMap.empty) ;;
let char_encode : int CharMap.t ref = ref (CharMap.empty) ;;
let int_encode : int SingleMap.t ref = ref (SingleMap.empty) ;;
let rat_encode : int PairsMap.t ref = ref (PairsMap.empty) ;;

let decode_one_exp x = match x with
  | (REF x) -> !(Array.get (!decode_array) x)
  | x -> x ;;

let rec int_decode_exp x =
    let e = decode_one_exp x
    in
        match e with
           | (APPL (f,l))      -> (APPL (f,List.map int_decode_exp l))
           | (QUANT (q,v,e,p)) -> (QUANT (q,v,int_decode_exp e,int_decode_exp p))
           | (LET (v,t,e,p))   -> (LET (int_decode_exp v,t,int_decode_exp e,int_decode_exp p))
           | (CASE (e,t,c))    -> (CASE (int_decode_exp e,t,List.map (fun (p,e) -> (int_decode_exp p,int_decode_exp e)) c))
           | (INDEX (e,i1,i2)) -> (INDEX (int_decode_exp e,i1,i2))
           | e                 -> e
   ;;

let rec decode_exp e = match e with
  | (REF x) -> !(Array.get (!rec_decode_array) x)
  | (APPL (f,l)) -> (APPL (f,List.map decode_exp l))
  | (QUANT (q,v,e,p)) -> (QUANT (q,v,decode_exp e,decode_exp p))
  | (LET (v,t,e,p)) -> (LET (decode_exp v,t,decode_exp e,decode_exp p))
  | (CASE (e,t,c)) -> (CASE (decode_exp e,t,List.map (fun (p,e) -> (decode_exp p,decode_exp e)) c))
  | (INDEX (e,i1,i2)) -> (INDEX (decode_exp e,i1,i2))
  | e -> e
  ;;

let rec has_special_construct e = match e with
  | (REF x) -> !(Array.get (!special_construct_array) x)
  | (APPL (f,l)) -> has_special_construct_list l
  | (QUANT (q,v,e,p)) -> true
  | (LET (v,t,e,p)) -> true
  | (CASE (e,t,c)) -> true
  | (INDEX (e,i1,i2)) -> true
  | e -> false
and has_special_construct_list l = match l with
  | [] -> false
  | (a::b) ->
    (has_special_construct a) || (has_special_construct_list b)
  ;;

let decode_two_exp x =
    let e = decode_one_exp x
    in
        match e with
           | (APPL (f,l))      -> (APPL (f,List.map decode_one_exp l))
           | (QUANT (q,v,e,p)) -> (QUANT (q,v,decode_one_exp e,decode_one_exp p))
           | (LET (v,t,e,p))   -> (LET (decode_one_exp v,t,decode_one_exp e,decode_one_exp p))
           | (CASE (e,t,c))    -> (CASE (decode_one_exp e,t,List.map (fun (p,e) -> (decode_one_exp p,decode_one_exp e)) c))
           | (INDEX (e,i1,i2)) -> (INDEX (decode_one_exp e,i1,i2))
           | e                 -> e
    ;;

let rec equal_sets f s = match (f,s) with
  | ([],[]) -> true
  | ([],x) -> false
  | ((f::r),x) ->
    if List.mem f x then
        equal_sets r (Mylist.delete_one f x)
    else
        false
  ;;

let ac_equal f s = match f s with
  | ((APPL (f1,l1)),(APPL (f2,l2))) ->
    if f1=f2 then equal_sets l1 l2 else false
  | (f,s) -> false

let addIntern exp sc =
    let r = !intern_count in
    let old_array_size = !array_size in
       (if (!intern_count)=(!array_size) then
           (array_size := (((!array_size) * 4) / 3) ;
            decode_array := Array.init (!array_size)
                            (fun (x) -> if x < old_array_size then
                                           Array.get (!decode_array) x
                                       else
                                           ref NOEXP) ;
            rec_decode_array := Array.init (!array_size)
                            (fun (x) -> if x < old_array_size then
                                           Array.get (!rec_decode_array) x
                                       else
                                           ref NOEXP) ;
            parent_array := Array.init (!array_size)
                            (fun (x) -> if x < old_array_size then
                                           Array.get (!parent_array) x
                                       else
                                           ref (IntListMap.empty)) ;
            special_construct_array := Array.init (!array_size)
                            (fun (x) -> if x < old_array_size then
                                           Array.get (!special_construct_array) x
                                       else
                                           ref false) ; ())
        else
            ()) ;
        intern_count := (!intern_count) + 1 ;
        (Array.get (!decode_array) r) := exp ;
        (Array.get (!rec_decode_array) r) := int_decode_exp exp ;
        (Array.get (!special_construct_array) r) := sc ;
        r
    ;;

let intern_nil_ac_symbol f =
    try
        REF (SingleMap.find f (!nil_ac_symbol_encode))
    with Not_found ->
        let x = addIntern (APPL (f,[])) false in
            nil_ac_symbol_encode := SingleMap.add f x (!nil_ac_symbol_encode);
            REF x
    ;;

let intern_nil_symbol f =
    try
        REF (SingleMap.find f (!nil_symbol_encode))
    with Not_found ->
        let x = addIntern (APPL (f,[])) false in
            nil_symbol_encode := SingleMap.add f x (!nil_symbol_encode);
            REF x
    ;;

let intern_var f =
    try
        REF (SingleMap.find f (!var_encode))
    with Not_found ->
        let x = addIntern (VAR f) false in
            var_encode := SingleMap.add f x (!var_encode);
            REF x
    ;;

let intern_marked_var f =
    try
        REF (SingleMap.find f (!marked_var_encode))
    with Not_found ->
        let x = addIntern (MARKED_VAR f) false in
            marked_var_encode := SingleMap.add f x (!marked_var_encode);
            REF x
    ;;

let intern_num f =
    try
        REF (SingleMap.find f (!int_encode))
    with Not_found ->
        let x = addIntern (NUM f) false in
            int_encode := SingleMap.add f x (!int_encode);
            REF x
    ;;

let intern_rat n d =
    try
        REF (PairsMap.find (n,d) (!rat_encode))
    with Not_found ->
        let x = addIntern (RATIONAL (n,d)) false in
            rat_encode := PairsMap.add (n,d) x (!rat_encode);
            REF x
    ;;

let intern_string f =
    try
        REF (StringMap.find f (!string_encode))
    with Not_found ->
        let x = addIntern (STRING f) false in
            string_encode := StringMap.add f x (!string_encode);
            REF x
    ;;

let intern_char f =
    try
        REF (CharMap.find f (!char_encode))
    with Not_found ->
        let x = addIntern (CHAR f) false in
            char_encode := CharMap.add f x (!char_encode);
            REF x
    ;;

let print_x = ref false ;;

let rec insert x l = match l with
  | [] -> [x]
  | (f::r) -> if x<f then x::f::r else f::(insert x r) ;;

let rec sort l = match l with
  | [] -> []
  | (f::r) -> insert f (sort r) ;;

let intern_vlist v = sort (List.map (fun (x,_) -> x) v)

let rec intern_exp is_ac exp =
    match exp with
       | (REF x) -> (REF x)
       | NOEXP -> REF 0
       | (APPL (f,[])) ->
          if is_ac f then
              intern_nil_ac_symbol f
          else
              intern_nil_symbol f
       | (APPL (f,l)) ->
         if is_ac f then
             intern_ac_appl is_ac f l
         else
             intern_appl (is_ac,f,l)
       | (NORMAL x) -> intern_exp is_ac x
       | (HIGHLIGHT x) -> intern_exp is_ac x
       | (VAR x) -> intern_var x
       | (MARKED_VAR x) -> intern_marked_var x
       | (NUM x) -> intern_num x
       | (RATIONAL (n,d)) -> intern_rat n d
       | (STRING x) -> intern_string x
       | (CHAR x) -> intern_char x
       | (QUANT (q,v,e,p)) -> intern_quant is_ac q v e p
       | (LET (v,t,e,p)) -> intern_let is_ac v t e p 
       | (CASE (e,t,c)) -> intern_case is_ac e t c
       | (INDEX (e,i1,i2)) -> intern_index is_ac e i1 i2
 and intern_index is_ac e i1 i2 =
     let REF e2 = intern_exp is_ac e in
     let parent_set1 = try (IntListMap.find [-2;0;0] (!(Array.get (!parent_array) e2))) with Not_found -> [] in
     let parent_set = List.filter (fun (x) -> (!(Array.get (!decode_array) x)) = (INDEX (REF e2,i1,i2))) parent_set1 in
         if parent_set=[] then
             let x = addIntern (INDEX (REF e2,i1,i2)) true in
	     let d = !(Array.get (!parent_array) e2) in
                 ((Array.get (!parent_array) e2) :=
                     IntListMap.add [-2;0;0] (x::parent_set1) d
                 ) ;
                 REF x
         else
             REF (List.hd parent_set)
 and intern_let is_ac v t e p =
     let REF e2 = intern_exp is_ac e in
     let REF p2 = intern_exp is_ac p in
     let REF v2 = intern_exp is_ac p in
     let parent_set1 = try (IntListMap.find [-1;p2;v2] (!(Array.get (!parent_array) e2))) with Not_found -> [] in
     let parent_set = List.filter (fun (x) -> (!(Array.get (!decode_array) x)) = (LET (REF v2,t,REF e2,REF p2))) parent_set1
     in
         if parent_set=[] then
             let x = addIntern (LET (REF v2,t,REF e2,REF p2)) true in
             let d = !(Array.get (!parent_array) e2) in
                 ((Array.get (!parent_array) e2) :=
                     IntListMap.add [-1;p2;v2]
                         (x::parent_set1) d
                 ) ;
                 REF x
         else
             REF (List.hd parent_set)
 and intern_case is_ac e t c =
     let REF e2 = intern_exp is_ac e in
     let c2 = List.map (fun (p,e) -> [intern_exp is_ac p;intern_exp is_ac e]) c in
     let c3 = List.map (fun ([p;e]) -> (p,e)) c2 in
     let c2 = (-2)::(List.map (fun (REF x) -> x) (List.fold_left List.append [] c2)) in
     let parent_set1 = try (IntListMap.find c2 (!(Array.get (!parent_array) e2))) with Not_found -> [] in
     let parent_set = List.filter (fun (x) -> (!(Array.get (!decode_array) x)) = (CASE (REF e2,t,c3))) parent_set1 in
         if parent_set=[] then
             let x = addIntern (CASE (REF e2,t,c3)) true in
             let d = !(Array.get (!parent_array) e2) in
                 ((Array.get (!parent_array) e2) :=
                     IntListMap.add c2 (x::parent_set1) d
                 ) ;
                 REF x
         else
             REF (List.hd parent_set)
 and intern_quant is_ac q v e p =
     let REF e2 = intern_exp is_ac e in
     let REF p2 = intern_exp is_ac p in
     let vl = intern_vlist v in
     let parent_set = try (IntListMap.find ([q;0;p2]@vl) (!(Array.get (!parent_array) e2))) with Not_found -> [] in
         if parent_set=[] then
             let x = addIntern (QUANT (q,v,REF e2,REF p2)) true in
             let d = !(Array.get (!parent_array) e2) in
                 ((Array.get (!parent_array) e2) :=
                     IntListMap.add ([q;0;p2]@vl) [x] d
                 ) ;
                 REF x
         else
             REF (List.hd parent_set)
 and intern_appl (is_ac,f,l) =
     let l3 = List.map (intern_exp is_ac) l in
     let l2 = List.map (fun (REF x) -> x) l3 in
     let parent_set = try (IntListMap.find (f::(List.tl l2)) (!(Array.get (!parent_array) (List.hd l2)))) with Not_found -> [] in
         if parent_set=[] then
             let x = addIntern (APPL (f,l3)) (has_special_construct_list l3) in
	     let d = !(Array.get (!parent_array) (List.hd l2)) in
                 ((Array.get (!parent_array) (List.hd l2)) :=
                     IntListMap.add (f::(List.tl l2))
                         [x] d
                 ) ;
                 REF x
         else
             REF (List.hd parent_set)
 and intern_ac_appl is_ac f l =
     let l2 = List.map (fun (x) -> let REF x=intern_exp is_ac x in x) l in
     let l2 = List.sort (fun x -> (fun y -> if x > y then 1 else (if x=y then 0 else -1))) l2 in
     let parent_set = try (IntListMap.find (f::(List.tl l2)) (!(Array.get (!parent_array) (List.hd l2)))) with Not_found -> [] in
         if parent_set=[] then
             let l3 = List.map (fun (x) -> REF x) l2 in
             let x = addIntern (APPL (f,l3)) (has_special_construct_list l3) in
             let d = !(Array.get (!parent_array) (List.hd l2)) in
             let find = try IntListMap.find (f::(List.tl l2)) d with Not_found -> [] in
                 ((Array.get (!parent_array) (List.hd l2)) :=
                         IntListMap.add (f::(List.tl l2)) (x::find) d) ;
                 REF x
         else
             REF (List.hd parent_set)
;;



