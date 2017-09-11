(******************************************************************************
 *                       
 * REWRITELIB
 *
 * kbrewrite.ml
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

(* require "list.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "trace.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "env.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "context.sml" ;  *)
(* require "crewrite.sml" ;  *)
(* require "kbrewr-s.sml" ;  *)
(* require "bmf.sml" ;  *)
(* require "match.sml" ;  *)
(* require "basis.__list" ;  *)
(* require "basis.__integer" ;  *)

(* open listimpl ; *)
open Exp ;;
(* open MATCHimpl ; *)
(* open TRACEimpl ; *)
(* open EXP_INTERNimpl ; *)
open Intern ;;
(* open ENVimpl ; *)
(* open SUBSTimpl ; *)
(* open CONTEXTimpl ; *)
(* open CREWRITEimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

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

let rec parent l = match l with
  | [x] -> []
  | [] -> []
  | (a::b) -> a::(parent b)
  ;;

let rec is_relevant_operator env e = match e with
  | (REF x) -> is_relevant_operator env (ExpIntern.decode_two_exp (REF x))
  | (APPL (f,[a;b])) ->
    (Env.isEQ env f) || (Env.isOrder env f)
  | _ -> false
  ;;

let rec is_relevant_term env e = match e with
  | (APPL (17,[x])) -> is_relevant_term env x
  | x -> is_relevant_operator env x
  ;;

let is_conjunction e = match e with
  | (APPL (9,l)) -> true
  | _ -> false
  ;;

let rec is_useful_conjunction env e = match e with
  | (REF x) -> is_useful_conjunction env (ExpIntern.decode_two_exp (REF x))
  | (APPL (9,l)) ->
    List.mem true (List.map (is_relevant_term env) l)
  | _ -> false
  ;;

let conjunction_p exp l = match l with
  | [] -> is_conjunction exp
  | x ->
    (is_conjunction (getSubterm exp x)) ||
        (match (getSubterm exp x) with
            | (APPL (17,[_])) ->
              (is_conjunction (getSubterm exp (parent x)))
            | _                  -> false)
  ;;

let default_operand e = match e with
  | (APPL (f,[APPL (t1,l1);APPL (t2,l2)])) ->
    t1=71 || t1=72 || t2=t1 || t2=72
  | (APPL (f,[_;APPL (t,l)])) ->
    t=71 || t=72
  | (APPL (f,[APPL (t,l);_])) ->
    t=71 || t=72
  | _ -> false
  ;;

let useful_subterm env exp t =
    (is_useful_conjunction env (getSubterm exp t)) ||
    ((is_relevant_operator env (getSubterm exp t)) &&
     not(conjunction_p exp (parent t)) &&
     not(default_operand (getSubterm exp t)))
  ;;

let possible_terms env e =
    List.filter (useful_subterm env e) (Exp.allSubterms e)
  ;;

let collect_terms env attrib f n =
    List.filter
    (fun ((Env.E e)::(Env.E v)::_) ->
        (match e with
          | (APPL (ff,l)) -> (f=ff) && ((n= -1) || (Env.isAC env f) || (Env.isC env f) || (Match.equal env (List.nth l n) v))
          | _             -> false
        )
    )
    (Env.selectAttrib env attrib f)
  ;;

let get_subst env p e =
    let mat = List.filter
                  (fun (x,y) -> y=[])
                  (Match.thematch env p e ) in
    let mat2 = List.map (fun (x,y) -> x) mat
    in
        (List.hd mat2)
    ;;

let test_subterm_relationship rewrite env (APPL (f,l)) n um cm =
    let
        (*val _ = print ("[Testing " ^ (prExp (APPL (f,l))) ^ " " ^ (Int.toString n) ^ " " ^ um ^ " " ^ cm ^ "]\n")*)
        mt_ops = List.map
                     (fun (x) -> (match x with
                                    | [_;_;(Env.S oo)] -> (oo,0)
                                    | [_;_;(Env.S oo);(Env.S eo)] -> (oo,eo))
                     )
                     (collect_terms env um f n) in
        (*val _ = print ("[" ^ (string_of_int (List.length mt_ops)) ^ "]\n")*)
    let ct_ops = List.map
                     (fun (x) -> (match x with
                                    | [_;_;(Env.S oo);_] -> (oo,0)
                                    | [_;_;(Env.S oo);(Env.S eo);_] -> (oo,eo))
                     )
                     (List.filter
                         (fun (ct) ->
                             (match (ct) with
                                  | [(Env.E e);_;_;(Env.E c)] ->
                                     List.mem
                                         (APPL (intern_true,[]))
                                         (rewrite
                                             env
                                             (Rsubst.subst
                                                 (get_subst env e (APPL (f,l)))
                                                 c
                                             )
                                         )
                                  | [(Env.E e);_;_;_;(Env.E c)] ->
                                     List.mem
                                         (APPL (intern_true,[]))
                                         (rewrite
                                             env
                                             (Rsubst.subst
                                                 (get_subst env e (APPL (f,l)))
                                                 c
                                             )
                                         )
                                | _                        -> false
                             )
                         )
                         (collect_terms env cm f n))
    in
        Mylist.remove_dups (mt_ops @ ct_ops)
  ;;

let rec upto n = match n with
  | (-1) -> []
  | 0 -> [0]
  | n -> n::(upto (n-1))
  ;;

let rec get_term_attrib env e = match e with
  | (APPL (17,[x])) ->
    get_term_attrib env x
  | (APPL (10,[a;b])) ->
    let (e1,o1,t1) = get_term_attrib env a in
    let (e2,o2,t2) = get_term_attrib env b
    in
        if (e1=0) then
            (e2,o2,t2)
        else
            (e1,o1,t1)
  | (APPL (f,l)) ->
    (match Env.getAttrib env intern_to [S(f)] with
       | [S(f);S(e)] -> (intern_to,f,e)
       | _ -> (
         match Env.getAttrib env intern_po [S(f)] with
             | [S(f);S(e)] -> (intern_po,f,e)
             | _ -> (
              match Env.getAttrib env intern_epo [S(f)] with
                 | [S(f);S(e)] -> (intern_epo,f,e)
                 | _ -> (
                   match Env.getAttrib env intern_eto [S(f)] with
                      | [S(f);S(e)] -> (intern_eto,f,e)
                      | _ -> (0,0,0)
    ))))
  | _ -> (0,0,0)
  ;;

let rec find_subterm x y v = match (x,y) with
  | ([],[]) -> v
  | ((a::b),(c::d)) ->
    if a=v then c else find_subterm b d v
  ;;
(*
fun get_inc_onestep (APPL (f,l)) =
    let val tu = collect_terms env intern_omi f (-1)
        val tc = collect_terms env intern_cmi f (-1)
    in

fun reduce_one_step env (APPL (f,[l,r])) =
    let
        val (t,oo,e) = get_term_attrib env (APPL (f,[l,r]))
        val (e,v) = if t=intern_po || t=intern_to then
                        get_onestep r
                    else
                        get_onestep l
    in
        if t=0 then
            (APPL (f,l))
        else
    end ;
*)
let rec break_term rewrite env e = match e with
  | (APPL (f,l)) ->
    let (*val _ = print ("[Break term " ^ (prExp (APPL (f,l))) ^ "]\n")*)
        ops =
            List.map (fun (n) -> ((test_subterm_relationship
                             rewrite env (APPL (f,l)) n intern_smi intern_csmi)@
                            (test_subterm_relationship
                             rewrite env (APPL (f,l)) n intern_omi intern_comi),
                            (test_subterm_relationship
                             rewrite env (APPL (f,l)) n intern_smd intern_csmd)@
                            (test_subterm_relationship
                             rewrite env (APPL (f,l)) n intern_omd intern_comd),
                            (test_subterm_relationship
                             rewrite env (APPL (f,l)) n intern_mi intern_cmi),
                            (test_subterm_relationship
                             rewrite env (APPL (f,l)) n intern_md intern_cmd)))
            (upto ((List.length l)-1))
    in
        break_list rewrite env (APPL (f,l)) l ops
  | (REF x) ->
    break_term rewrite env (ExpIntern.decode_one_exp (REF x))
  | x -> []
and break_list rewrite env e l1 l2 = match (l1,l2) with
  | ([],[]) -> []
  | ((a::b),(([],[],[],[])::r)) ->
    break_list rewrite env e b r
  | ((a::b),((smi,smd,mi,md)::r)) ->
    (break_term rewrite env a)@
    (List.map (fun (n,eq) -> (APPL (n,[a;e]))) smi)@
    (List.map (fun (n,eq) -> (APPL (n,[e;a]))) smd)@
    (List.map (fun (n,eq) -> (APPL (intern_or,[APPL (eq,[a;e]);APPL (n,[a;e])]))) mi)@
    (List.map (fun (n,eq) -> (APPL (intern_or,[APPL (eq,[e;a]);APPL (n,[e;a])]))) md)@
    (break_list rewrite env e b r)
  ;;

let break_down_op rewrite env e = match e with
  | (APPL (f,[a;b])) ->
    if is_relevant_operator env (APPL (f,[a;b])) then
        (APPL (f,[a;b]))::(((break_term rewrite env a)@(break_term rewrite env b)))
    else
        []
  | (APPL (17,[APPL (f,[a;b])])) ->
    if is_relevant_operator env (APPL (f,[a;b])) then
        ((APPL (17,[APPL (f,[a;b])]))::((break_term rewrite env a)@(break_term rewrite env b)))
    else
        []
  | x -> []
  ;;

let rec decode_term x =
    match ExpIntern.decode_one_exp x with
       | (APPL (17,[x])) -> (APPL (17,[decode_term x]))
       | (APPL (9,[a;b])) -> (APPL (9,[decode_term a;decode_term b]))
       | (APPL (10,[a;b])) -> (APPL (10,[decode_term a;decode_term b]))
       | x -> x
  ;;

let break_down_ops rewrite env e = match e with
  | (APPL (9,l)) ->
    List.fold_right List.append (List.map (fun (x) -> break_down_op rewrite env (decode_term x)) l) []
  | x -> break_down_op rewrite env (decode_term x) ;;

let rec binary_term e = match e with
  | (APPL (9,[a;b])) ->
    (binary_term a) && (binary_term b)
  | (APPL (10,[a;b])) ->
    (binary_term a) && (binary_term b)
  | (APPL (f,[a;b])) -> true
  | (APPL (17,[a])) -> binary_term a
  | _ -> false
  ;;

let get_env_terms env vars =
    let rules = List.map
                    (fun (x) -> ExpIntern.decode_exp (REF x))
                    (Env.getContextList env) in
    (*let _ = print "Rules:\n" in
    let _ = map (fn (x) => print ("    rule " ^ (prExp x) ^ "\n")) rules in*)
    let rules = (List.filter
            (fun (x) ->
                (match x with
                   | (APPL (1,[l;(APPL (4,[]));(APPL (4,[]))])) -> true
                   | (APPL (1,[l;(APPL (5,[]));(APPL (4,[]))])) -> true
                   | (APPL (1,[l;(APPL (17,[APPL (5,[])]));(APPL (4,[]))])) -> true
                   | (APPL (1,[l;(APPL (17,[APPL (4,[])]));(APPL (4,[]))])) -> true
                   | _ -> false)) rules)
    in
        (List.filter binary_term
            (List.map
                (fun (x) -> (match x with
                                    | (APPL (f,[l;(APPL (4,[]));c])) -> Rcontext.unmarkVars l
                                    | (APPL (f,[l;(APPL (5,[]));c])) -> (APPL (intern_not,[Rcontext.unmarkVars l]))
                                    | (APPL (f,[l;(APPL (17,[APPL (5,[])]));c])) -> Rcontext.unmarkVars l
                                    | (APPL (f,[l;(APPL (17,[APPL (4,[])]));c])) -> (APPL (17,[Rcontext.unmarkVars l]))))
               (List.filter
                    (fun (x) -> not((Mylist.intersect (Rcontext.getMarkedVars x) vars)=[])) rules)))
    ;;

let is_lt (t,oo,e) ee = match ee with
  | (APPL (9,[(APPL (f1,l1));(APPL (17,[APPL (f2,l2)]))])) ->
    (t=intern_eto || t=intern_epo) && f1=oo && f2=e
  | (APPL (9,[(APPL (17,[APPL (f2,l1)]));(APPL (f1,l2))])) ->
    (t=intern_eto || t=intern_epo) && f1=oo && f2=e
  | (APPL (17,[APPL (f,l)])) ->
    (t=intern_eto && oo=f)
  | (APPL (f,l)) ->
    (t=intern_to || t=intern_po) && f=oo
  | _ -> false
  ;;

let mk_lt (t,oo,e) l r =
    if t=intern_eto then
        (APPL (intern_not,[APPL (oo,[r;l])]))
    else if t=intern_epo then
        (APPL (intern_and,[APPL (oo,[l;r]);APPL (intern_not,[APPL (e,[l;r])])]))
    else
        (APPL (oo,[l;r]))
  ;;

let is_le (t,oo,e) ee = match ee with
  | (APPL (10,[APPL (f1,l1);APPL (f2,l2)])) ->
    (t=intern_to || t=intern_po) && ((f1=oo && f2=e) || (f1=e && f2=oo))
  | (APPL (17,[APPL (f,l)])) ->
    (t=intern_to && f=oo)
  | (APPL (f,l)) ->
    (t=intern_eto || t=intern_epo) && f=oo
  | _ -> false
  ;;

let mk_le (t,oo,e) l r =
    if t=intern_to then
        (APPL (intern_not,[APPL (oo,[r;l])]))
    else if t=intern_po then
        (APPL (intern_or,[APPL (oo,[l;r]);APPL (e,[l;r])]))
    else
        (APPL (oo,[l;r]))
  ;;

let is_eq (t,oo,e) ee = match ee with
  | (APPL (f,l)) -> e=f
  | _ -> false
  ;;

let mk_eq (t,oo,e) l r = (APPL (e,[l;r])) ;;

let is_ne (t,oo,e) ee = match ee with
  | (APPL (17,[APPL (f,l)])) -> e=f
  | _ -> false
  ;;

let mk_ne (t,oo,e) l r = (APPL (intern_not,[APPL (e,[l;r])]))
  ;;

let rec get_left_operand (e,oo,t) ee = match ee with
  | (APPL (f,[])) -> (REF 0)
  | (APPL (9,[APPL (f,l);a])) ->
    if f=e then
        get_left_operand (e,oo,t) a
    else
        get_left_operand (e,oo,t) (APPL (f,l))
  | (APPL (10,[APPL (f,l);a])) ->
    if f=e then
        get_left_operand (e,oo,t) a
    else
        get_left_operand (e,oo,t) (APPL (f,l))
  | (APPL (17,[x])) -> get_right_operand (e,oo,t) x
  | (APPL (f,[a;b])) -> a
  (*| get_left_operand _ x = (print ((prExp x) ^ "\n") ; x)*)
and get_right_operand (e,oo,t) ee = match ee with
  | (APPL (f,[])) -> (REF 0)
  | (APPL (9,[APPL (f,l);a])) ->
    if f=e then
        get_right_operand (e,oo,t) a
    else
        get_right_operand (e,oo,t) (APPL (f,l))
  | (APPL (10,[APPL (f,l);a])) ->
    if f=e then
        get_right_operand (e,oo,t) a
    else
        get_right_operand (e,oo,t) (APPL (f,l))
  | (APPL (17,[x])) -> get_left_operand (e,oo,t) x
  | (APPL (f,[a;b])) -> b
  (*| get_right_operand _ x = (print ((prExp x) ^ "\n") ; x)*)
  ;;

let rec intern_term env e = match e with
  | (APPL (17,[x])) ->
    (APPL (17,[intern_term env x]))
  | (APPL (9,[a;b])) ->
    (APPL (10,[intern_term env a;intern_term env b]))
  | (APPL (10,[a;b])) ->
    (APPL (9,[intern_term env a;intern_term env b]))
  | (REF x) -> intern_term env (ExpIntern.decode_one_exp (REF x))
  | x ->
    ExpIntern.decode_one_exp
        (ExpIntern.intern_exp (Env.isACorC env) x) ;;

let and_combine_operands oper op1 op2 =
    let l1 = get_left_operand oper op1 in
    let r1 = get_right_operand oper op1 in
    let l2 = get_left_operand oper op2 in
    let r2 = get_right_operand oper op2 in
        if is_eq oper op1 then
           (if is_ne oper op2 then
                (APPL (intern_false,[]))
            else if is_le oper op2 then
                op1
            else if is_lt oper op2 then
                (APPL (intern_false,[]))
            else
                op1)
        else if is_ne oper op1 then
           (if is_ne oper op2 then
                op1
            else if is_eq oper op2 then
                (APPL (intern_false,[]))
            else if is_le oper op2 then
                mk_lt oper l2 r2
            else
                op2)
        else if is_lt oper op1 then
           (if is_ne oper op2 then
                op1
            else if is_eq oper op2 then
                (APPL (intern_false,[]))
            else if is_lt oper op2 then
               (if l1=l2 then
                    op1
                else
                    (APPL (intern_false,[])))
            else
               (if l1=l2 then
                    op1
                else
                    (APPL (intern_false,[]))))
        else
           (if is_ne oper op2 then
                mk_lt oper l1 r1
            else if is_eq oper op2 then
                op2
            else if is_lt oper op2 then
               (if l1=l2 then
                    op2
                else
                    (APPL (intern_false,[])))
            else
               (if l1=l2 then
                    op1
                else
                    mk_eq oper l1 r1))
    ;;

let same_operands oper op1 op2 =
    ((get_left_operand oper op1=get_left_operand oper op2) &&
     (get_right_operand oper op1=get_right_operand oper op2)) ||
    ((get_left_operand oper op1=get_right_operand oper op2) &&
     (get_right_operand oper op1=get_left_operand oper op2))
  ;;

let can_combine oper op1 op2 =
    (is_le oper op1 || is_lt oper op1 || is_eq oper op1 || is_ne oper op1) &&
    (is_le oper op2 || is_lt oper op2 || is_eq oper op2 || is_ne oper op2) &&
    same_operands oper op1 op2
  ;;

let elaborate_phase env oper terms =
    let _ = Rtrace.trace_list "kbrewrite" (fun x -> (List.map (fun (x) -> ("elaborate term = " ^ (prExp (intern_term env x)))) terms)) in
    let terms = List.map (intern_term env) terms in
        (*val _ = print "Combine"
        _ = flush_out std_out*)
    let _ = Rtrace.trace_list "kbrewrite" (fun x -> (List.map (fun (x) -> ("elaborate term2 = " ^ (prExp (intern_term env x)))) terms)) in
    let rec add_terms oo t l = match l with
          | [] -> (oo,t)
          | (f::rest) ->
            let f = intern_term env f in
                (*val _ = print ("t1 = " ^ (prExp f) ^ "\n")*)
            let (REF l) = get_left_operand oper f in
            let (REF r) = get_right_operand oper f in
            let (REF n) = ExpIntern.intern_exp (Env.isACorC env) f in
            let lv = try SingleMap.find l oo with Not_found -> [] in
            let rv = try SingleMap.find r oo with Not_found -> [] in
            let dv = try PairsMap.find (l,r) t with Not_found -> [] in
            let nlv = if List.mem n lv then lv else n::lv in
            let nrv = if List.mem n rv then rv else n::rv in
            let ndv = if List.mem n dv then dv else n::dv in
                add_terms
                    (SingleMap.add r nrv (SingleMap.add l nlv oo))
                    (PairsMap.add (l,r) ndv t)
                    rest in
    let rec delete_terms oo t l = match l with
          | [] -> (oo,t)
          | (f::rest) ->
            let f = intern_term env f in
                (*val _ = print ("t2 = " ^ (prExp f) ^ "\n")*)
            let (REF l) = get_left_operand oper f in
            let (REF r) = get_right_operand oper f in
            let (REF n) = ExpIntern.intern_exp (Env.isACorC env) f in
            let lv = try SingleMap.find l oo with Not_found -> [] in
            let rv = try SingleMap.find r oo with Not_found -> [] in
            let dv = try PairsMap.find (l,r) t with Not_found -> [] in
            let nlv = if List.mem n lv then Mylist.delete n lv else lv in
            let nrv = if List.mem n rv then Mylist.delete n rv else rv in
            let ndv = if List.mem n dv then Mylist.delete n dv else dv in
                delete_terms
                    (SingleMap.add r nrv (SingleMap.add l nlv oo))
                    (PairsMap.add (l,r) ndv t)
                    rest in
    let (one_ref,two_ref) = add_terms 
                    (SingleMap.empty)
                    (PairsMap.empty) terms in
        (*val _ = print "Combine\n"
        val _ = flush_out std_out*)
    let rec combine_all l = match l with
          | [x] -> x
          | (f::r) ->
            and_combine_operands oper f (combine_all r) in
    let rec combine_terms one_ref two_ref =
            PairsMap.fold (fun (f,s) -> (fun nl -> (fun (st,dt) ->
                if List.length nl < 2 then
                    (st,dt)
                else let terms = (List.map (fun (x) -> decode_term (REF x)) nl) in
                     let (REF nnew) = ExpIntern.intern_exp (Env.isACorC env) (combine_all terms) in
                     let (st,dt) = delete_terms st dt (List.map (fun (x) -> REF x) nl) in
                     let (st,dt) = add_terms st dt [(REF nnew)]
                     in
                         (st,dt)
                     ))) two_ref (one_ref,two_ref) in
    let rec combine_rev_terms one_ref two_ref =
            PairsMap.fold (fun (f,s) -> (fun nl -> (fun (st,dt) ->
                if List.length nl==0 then (st,dt) else
                (*let _ = Rtrace.trace_list "kbrewrite" (fun x -> (List.map (fun y -> "tt " ^ (prExp (decode_term (REF y)))) nl)) in*)
                (*let _ = Rtrace.trace "kbrewrite" (fun x -> "length " ^ (string_of_int (List.length nl))) in*)
                let [term] = (List.map (fun (x) -> decode_term (REF x)) nl) in
                let (REF l) = get_left_operand oper term in
                let (REF r) = get_right_operand oper term in
                let rps = try PairsMap.find (r,l) dt with Not_found -> [] in
                    if rps=[] then (st,dt)
                    else let [rt] = rps in
                         let REF (nnew) = ExpIntern.intern_exp (Env.isACorC env) (and_combine_operands oper (decode_term (REF rt)) term) in
                         (*let _ = Rtrace.trace "kbrewrite" (fun x -> "found pair " ^ (prExp term) ^ " " ^ (prExp (decode_term (REF rt)))) in*)
                         let (st,dt) = delete_terms st dt (List.map (fun (x) -> REF x) nl) in
                         let (st,dt) = add_terms st dt [(REF nnew)] in
                             (st,dt)))) two_ref (one_ref,two_ref) in
    let (one_ref,two_ref) = combine_terms one_ref two_ref in
    let (one_ref,two_ref) = combine_rev_terms one_ref two_ref in
        (*val _ = print "Loop\n"
        val _ = flush_out std_out*)
        (*val _ = trace_list "kbrewrite" (fn (e) => map (fn (x,_) => ("start " ^ (prExp x))) terms)
        val _ = trace_list "kbrewrite" (fn (e) => map (fn (x,_) => ("combined " ^ (prExp x))) combined)*)

    let make_tran_op1 make op1 op2 =
            (*let _ = Rtrace.trace "kbrewrite" (fun x -> ("t3 = " ^ (prExp op1))) in*)
            (*let _ = Rtrace.trace "kbrewrite" (fun x -> ("t4 = " ^ (prExp op2))) in*)
            let l1 = get_left_operand oper op1 in
            let l2 = get_left_operand oper op2 in
            let r1 = get_right_operand oper op1 in
            let r2 = get_right_operand oper op2 in
                (if l1=l2 then [make oper r2 r1] else []) @
                (if l1=r2 then [make oper l2 r1] else []) @
                (if r1=l2 then [make oper l1 r2] else []) @
                (if r1=r2 then [make oper l1 l2] else []) in

    let make_tran_op2 make op1 op2 =
            (*let _ = Rtrace.trace "kbrewrite" (fun x -> ("t5 = " ^ (prExp op1))) in*)
            (*let _ = Rtrace.trace "kbrewrite" (fun x -> ("t6 = " ^ (prExp op2))) in*)
            let l1 = get_left_operand oper op1 in
            let l2 = get_left_operand oper op2 in
            let r1 = get_right_operand oper op1 in
            let r2 = get_right_operand oper op2 in
                (if l1=l2 then [make oper r1 r2] else []) @
                (if l1=r2 then [make oper l2 r1] else []) @
                (if r1=l2 then [make oper l1 r2] else []) @
                (if r1=r2 then [make oper l2 l1] else [])
            in

    let make_dir_tran_op make op1 op2 =
            (*let _ = Rtrace.trace "kbrewrite" (fun x -> ("t7 = " ^ (prExp op1))) in*)
            (*let _ = Rtrace.trace "kbrewrite" (fun x -> ("t8 = " ^ (prExp op2))) in*)
            let l1 = get_left_operand oper op1 in
            let l2 = get_left_operand oper op2 in
            let r1 = get_right_operand oper op1 in
            let r2 = get_right_operand oper op2 in
                (if l1=r2 then [make oper l2 r1] else []) @
                (if r1=l2 then [make oper l1 r2] else [])
            in

    let closure_terms op1 op2 =
           Rtrace.trace "kbrewrite" (fun x -> "closure_terms " ^ (prExp op1) ^ " " ^ (prExp op2)) ;
           let (a,b,c) = oper in Rtrace.trace "kbrewrite" (fun x -> "oper " ^ (Intern.decode a) ^ " " ^ (Intern.decode b) ^ " " ^ (Intern.decode c));
           if is_eq oper op1 then
               (if is_eq oper op2 then
                    make_tran_op2 mk_eq op1 op2
                else if is_ne oper op2 then
                    make_tran_op2 mk_ne op1 op2
                else if is_lt oper op2 then
                    make_tran_op2 mk_lt op1 op2
                else if is_le oper op2 then
                    make_tran_op2 mk_le op1 op2
                else
                    [])
           else if is_ne oper op1 then
               (if is_eq oper op2 then
                    make_tran_op2 mk_ne op1 op2
                else
                    [])
           else if is_lt oper op1 then
               (if is_eq oper op2 then
                    make_tran_op1 mk_lt op1 op2
                else if is_lt oper op2 then
                    make_dir_tran_op mk_lt op1 op2
                else if is_le oper op2 then
                    make_dir_tran_op mk_lt op1 op2
                else
                    [])
           else if is_le oper op1 then
               (if is_eq oper op2 then
                    make_tran_op1 mk_lt op1 op2
                else if is_lt oper op2 then
                    make_dir_tran_op mk_lt op1 op2
                else if is_le oper op2 then
                    make_dir_tran_op mk_le op1 op2
                else
                    [])
           else
               [] in
    let rec transitive_cycle op1 l = match l with
          | [] -> []
          | (op2::r) ->
            (closure_terms op1 op2) @ (transitive_cycle op1 r) in
    let rec new_transitive_terms sd dd terms =
            List.fold_right List.append (List.map (fun (term) ->
                 let (REF l) = get_left_operand oper term in
                 let (REF r) = get_right_operand oper term in
                 let t1 = SingleMap.find l sd in
                 let t2 = SingleMap.find r sd in
                 let t = t1@t2 in
                 let terms = List.map (fun (x) -> decode_term (REF x)) t in
                     transitive_cycle term terms
                 ) terms) [] in

    let rec transitive_closure sd dd terms =
            let
                (*val _ = trace_list "kbrewrite" (fn (e) => map (fn (x,_) => ("l " ^ (prExp x))) l)*)
                rec represented_by e f = match (e,f) with
                  | (_,(APPL (5,[]))) -> true
                  | ((APPL (5,[])),_) -> false
                  | (op1,op2) ->
                    (same_operands oper op1 op2) &&
                    (((is_ne oper op1) && ((is_ne oper op2) || (is_lt oper op2))) ||
                     ((is_lt oper op1) && (is_lt oper op2)) ||
                     ((is_eq oper op1) && (is_eq oper op2)) ||
                     ((is_le oper op1) && ((is_le oper op2) || (is_lt oper op2) || (is_eq oper op2)))) in
            let rec member x dd =
                let (REF left) = get_left_operand oper x in
                let (REF right) = get_right_operand oper x in
                let nl1 = try PairsMap.find (left,right) dd with Not_found -> [] in
                let nl2 = try PairsMap.find (right,left) dd with Not_found -> [] in
                let nl2 = List.filter (fun x -> is_eq oper (decode_term (REF x))) nl2 in
                let nl = nl1@nl2 in
                let terms = List.map (fun (x) -> decode_term (REF x)) nl in
                    not((List.filter (fun (t) -> represented_by x t) terms)=[]) in
            let nt = new_transitive_terms sd dd terms in
            let _ = Rtrace.trace "kbrewrite" (fun x -> "    Cycle") in
            let _ = Rtrace.trace_list "kbrewrite" (fun xx -> (List.map (fun (x) -> ("    b " ^ (prExp x))) nt)) in
            let nt1 = List.map (fun (REF x) -> x)
                          (List.map (ExpIntern.intern_exp (Env.isACorC env)) nt)
 in
            let ntdict = List.fold_right (fun e -> (fun d -> SingleMap.add e true d)) nt1 (SingleMap.empty) in
            let nt = List.map
                         (fun (a,b) -> intern_term env (REF a))
                         (SingleMap.bindings ntdict) in
            let new_trans = List.filter (fun (x) -> not(member x dd)) nt in
            let _ = Rtrace.trace_list "kbrewrite" (fun x -> List.map (fun (x) -> ("    a " ^ (prExp x))) new_trans) in
                if new_trans=[] then
                    (sd,dd)
                else
                    (let (sd,dd) = add_terms sd dd new_trans in
                     let res = List.fold_left List.append [] (List.map (fun (_,t) -> t) (PairsMap.bindings dd)) in
                     let _ = Rtrace.trace "kbrewrite" (fun x -> "ta1 count = " ^ (string_of_int (List.length res))) in
                     let _ = Rtrace.trace_list "kbrewrite" (fun x -> List.map (fun (x) -> ("    ta1 = " ^ (prExp (decode_term (REF x))))) res) in
                     let (sd,dd) = combine_terms sd dd in
                     let (sd,dd) = combine_rev_terms sd dd in
                     let res = List.fold_left List.append [] (List.map (fun (_,t) -> t) (PairsMap.bindings dd)) in
                     let _ = Rtrace.trace_list "kbrewrite" (fun (x) -> List.map (fun (x) -> ("    t2 = " ^ (prExp (decode_term (REF x))))) res) in
                     let rr = List.map (fun x -> decode_term (REF x)) res in
                        if List.mem (APPL (intern_false,[])) rr then
                            (sd,dd)
                        else
                            transitive_closure sd dd new_trans) in
        let (sd,dd) = transitive_closure one_ref two_ref terms in
        let res = List.fold_right List.append (List.map (fun (_,t) -> t) (PairsMap.bindings dd)) [] in
        let _ = Rtrace.trace "kbrewrite" (fun (x) -> "Elaboration") in
        let _ = Rtrace.trace_list "kbrewrite" (fun q -> List.map (fun (x) -> ("    t = " ^ (prExp (decode_term (REF x))) ^ "\n")) res) in
        (List.map (fun (x) -> decode_term (REF x)) res)
    ;;

let has_change = ref false ;;

let rec rewrite_op (t,c,e) env cond_terms terms =
    let _ = Rtrace.trace "kbrewrite" (fun (x) -> ("Operator " ^ (decode t) ^ " " ^ (decode c) ^ " " ^ (decode e))) in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("cond_term " ^ (prExp x))) cond_terms) in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("terms " ^ (prExp x))) terms) in
    let terms = List.map (intern_term env) terms in
        (*val _ = print "elaborate1\n"
        val _ = flush_out std_out*)
    let results1 = elaborate_phase env (t,c,e) cond_terms in
        (*val _ = print "elaborate2\n"
        val _ = flush_out std_out*)
    let results2 = elaborate_phase env (t,c,e) (terms@cond_terms) in
        (*val _ = print "elaborate3\n"
        val _ = flush_out std_out*)
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("results1 " ^ (prExp x))) results1) in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("results2 " ^ (prExp x))) results2) in
    let rec strengthen_term l term = match l with
          | [] -> term
          | (a::b) ->
            if (can_combine (t,c,e) a term) && not(a=term) then
                (has_change := true ; a)
            else
                strengthen_term b term
    in
        if List.mem (APPL (intern_false,[])) results1 then
            (Rtrace.trace "kbrewrite" (fun x -> "True case");
            (has_change := true ; [[APPL (intern_true,[])]]))
        else if List.mem (APPL (intern_false,[])) results2 then
            (Rtrace.trace "kbrewrite" (fun x -> "False case");
            (has_change := true ; [[APPL (intern_false,[])]]))
        else
            let _ = Rtrace.trace "kbrewrite" (fun x -> "Default case") in
            let r =
            [(List.filter
                 (fun (x) -> if List.mem x results1 then (if List.mem x terms then ((has_change := true) ; false) else false) else true)
                 (List.map
                     (strengthen_term results2)
                     terms))] in
            let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("return " ^ (prExp x))) (List.hd r)) in
                r
 
    ;;

let rec make_builtins l = match l with
  | ((NUM n)::r) ->
    (make_num_builtins n r)@(make_builtins r)
  | ((STRING s)::r) ->
    (make_string_builtins s r)@(make_builtins r)
  | ((CHAR c)::r) ->
    (make_char_builtins c r)@(make_builtins r)
  | (f::r) -> make_builtins r
  | [] -> []
and make_num_builtins n1 l = match l with
  | ((NUM n2)::r) ->
    if n1<n2 then
        (APPL (intern_less,[NUM n1;NUM n2]))::(APPL (intern_preceq,[NUM n1;NUM n2]))::(make_num_builtins n1 r)
    else if n2<n1 then
        (APPL (intern_less,[NUM n2;NUM n1]))::(APPL (intern_preceq,[NUM n2;NUM n1]))::(make_num_builtins n1 r)
    else
        make_num_builtins n1 r
  | (_::r) -> make_num_builtins n1 r
  | [] -> []
and make_string_builtins n1 l = match l with
  | ((STRING n2)::r) ->
    if n1<n2 then
        (APPL (intern_preceq,[STRING n1;STRING n2]))::(make_string_builtins n1 r)
    else if n2<n1 then
        (APPL (intern_preceq,[STRING n2;STRING n1]))::(make_string_builtins n1 r)
    else
        make_string_builtins n1 r
  | (_::r) -> make_string_builtins n1 r
  | [] -> []
and make_char_builtins n1 l = match l with
  | ((CHAR n2)::r) ->
    if n1<n2 then
        (APPL (intern_preceq,[CHAR n1;CHAR n2]))::(make_char_builtins n1 r)
    else if n2<n1 then
        (APPL (intern_preceq,[CHAR n2;CHAR n1]))::(make_char_builtins n1 r)
    else
        make_char_builtins n1 r
  | (_::r) -> make_char_builtins n1 r
  | [] -> [] ;;

let rec get_constants e = match e with
  | (NUM n) -> [NUM n]
  | (STRING s) -> [STRING s]
  | (CHAR c) -> [CHAR c]
  | (APPL (f,l)) -> List.fold_right List.append (List.map get_constants l) []
  | (REF x) -> get_constants (ExpIntern.decode_exp (REF x))
  | _ -> [] ;;

let main_rewrite_loop rewrite env exp =
    let terms = List.filter binary_term (break_down_ops rewrite env exp) in
    let _ = Rtrace.trace "kbrewrite" (fun (xx) -> "KB Rewriting " ^ (prExp exp) ^ " " ^ (string_of_int (List.length terms)) ^ " " ^ (string_of_int (List.length (break_down_ops rewrite env exp)))) in
    let _ = Rtrace.indent () in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("term " ^ (prExp x))) terms) in
    let env_terms = get_env_terms env (Rcontext.getFreeVars (ExpIntern.decode_exp exp)) in
        (*val _ = map (fn (x) => print ("env " ^ (prExp x) ^ "\n")) env_terms*)
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("env_term " ^ (prExp x))) env_terms) in
    let env_op_terms =
            (List.filter binary_term (List.fold_right List.append (List.map (break_down_ops rewrite env) env_terms) [])) in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("env_op_term " ^ (prExp x))) env_op_terms) in
    let cond_terms = env_terms@env_op_terms in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("cond_term " ^ (prExp x))) cond_terms) in
    let exp_terms1 = (match exp with
                           | (APPL (9,l)) -> l
                           | _            -> [exp]) in
    let exp_terms = List.filter binary_term exp_terms1 in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("exp_term " ^ (prExp x))) exp_terms) in
    let all_terms = (exp_terms@cond_terms) in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("all_term " ^ (prExp x))) all_terms) in
    let operands = List.fold_right List.append (List.map get_constants all_terms) [] in
    let builtin_terms = make_builtins operands in
    let cond_terms = cond_terms@builtin_terms in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("cond_term2 " ^ (prExp x))) cond_terms) in
    let residue = Mylist.difference exp_terms1 exp_terms in
    let _ = Rtrace.trace_list "kbrewrite" (fun (e) -> List.map (fun (x) -> ("residue " ^ (prExp x))) residue) in
    let ops = (List.filter
                   (fun (t,_,_) -> not(t=0))
                   (Mylist.remove_dups
                       (List.map (get_term_attrib env) (terms@env_terms@exp_terms)))) in
    let _ = (has_change := false) in
    let res = List.map (fun (x) -> (match (x@residue) with
                                     | [a] -> a
                                     | [] -> (APPL (intern_true,[]))
                                     | _   -> (APPL (intern_and,(x@residue))))
                      )
                      (List.fold_right
                          (fun oper -> (fun expl ->
                              List.fold_right List.append (List.map (rewrite_op oper env cond_terms) expl) []
                          ))
                          ops [exp_terms]) in
    let _ = Rtrace.trace "kbrewrite" (fun (xx) -> "Results: " ^ (prExp exp)) in
    let _ = Rtrace.indent () in
    let _ = Rtrace.trace_list "kbrewrite" (fun (xx) -> List.map prExp res) in
    let _ = Rtrace.undent () in
    let _ = Rtrace.undent () in
        if res=[exp] then [] else if (!has_change) then res else []
    ;;

let rewritexx rewrite env e =
    let res = rewrite env e in
        if res=[] then
            [e]
        else
            res
    ;;

let rec build_env rewrite env e l = match l with
  | [] -> env
  | (a::b) ->
    build_env rewrite (Crewrite.create_rules (rewritexx rewrite env) env e a) (getSubterm e [a]) b
  ;;

let rec prIlist l = match l with
  | [] -> ""
  | [a] -> (string_of_int a)
  | (a::b) -> (string_of_int a) ^ "," ^ (prIlist b) ;;

let make_cond_env rewrite env e sub =
    let
        (*val _ = print ("[creating env " ^ (prExp e) ^ " " ^ (prIlist sub) ^ "]\n")*)
        env1 = build_env rewrite env e sub
    in
        env1
    ;;

let in_kb = ref false ;;

let kbrewrite rewrite e x t =
    let _ = (in_kb := true) in
        (*val _ = print "start kbrewrite\n"
        val _ = TextIO.flushOut TextIO.stdOut*)
    let r = List.filter
                (fun (xx) -> not(Match.equal e x xx))
                (List.map (replaceSubterm x t) (main_rewrite_loop rewrite (make_cond_env rewrite e x t) (getSubterm x t))) in
        (*val _ = print "finish kbrewrite\n"
        val _ = TextIO.flushOut TextIO.stdOut*)
    let _ = (in_kb := false)
    in
        r
  ;;

let kbrewrite2 rewrite e x t =
    if (!in_kb) then
        []
    else
        let _ = (in_kb := true) in
        (*let _ = print_string ("start kbrewrite " ^ (prExp (ExpIntern.decode_exp x)) ^ "\n") in*)
            (*val _ = TextIO.flushOut TextIO.stdOut*)
        let r = (List.filter
                    (fun (xx) -> Match.equal_smaller e xx x)
                    (List.map (replaceSubterm x t) (main_rewrite_loop rewrite (make_cond_env rewrite e x t) (getSubterm x t)))) in
            (*val _ = print "finish kbrewrite\n"
            val _ = TextIO.flushOut TextIO.stdOut*)
        let _ = (in_kb := false) in
            r
  ;;




