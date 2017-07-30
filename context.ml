(******************************************************************************
 *
 * REWRITELIB
 *
 * context.ml
 * 
 * Routines for assisting in the treatment of bound variable contexts
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
(* require "intern.sml" ;  *)
(* require "list.sml" ;  *)
(* require "contex-s.sml" ;  *)
(* require "basis.__integer" ;  *)

open Exp ;;
(* open INTERNimpl ; *)
(* open listimpl ; *)
(* open EXP_INTERNimpl ; *)

let rec strip l = match l with
  | [] -> []
  | ((a,b)::r) -> a::(strip r) ;;

let rec getFVars b e = match e with
  | (VAR v) -> if List.mem v b then [] else [v]
  | (APPL(s,l)) ->
    List.fold_right List.append (List.map (fun x -> getFVars b x) l) []
  | (QUANT (q,bb,e,p)) ->
    List.fold_right List.append (List.map (fun x -> getFVars (List.append (strip bb) b) x)
                     ([e;p])) []
  | (LET (v,t,bb,e)) ->
    List.append (getFVars b bb) (getFVars (List.append b (getFVars b v)) e)
  | (CASE (e,t,c)) ->
    (List.append (getFVars b e)
             (List.fold_left List.append [] (List.map (fun (p,e) -> getFVars (List.append b (getFVars [] p)) e) c)))
  | (NORMAL (e)) -> getFVars b e
  | (REF x) -> getFVars b (ExpIntern.decode_exp (REF x))
  | (x) -> [] ;;

let getFreeVars e = Mylist.remove_dups (getFVars [] e) ;;

let rec getFreeVarsWI b e = match e with
  | (VAR v) ->
    if List.mem v b then [] else [(v,[])]
  | (APPL(s,l)) ->
    let (_,v) = List.fold_left (fun (n,l) -> (fun e ->
                            (n+1,(List.map (fun (a,b) -> (a,n::b))
                                (getFreeVarsWI b e))
                            @l)
                        ))
                        (0,[])
                        l
    in
        v
  | (QUANT (q,bb,e,p)) ->
    (List.map (fun (a,b) -> (a,0::b)) (getFreeVarsWI b e)) @
    (List.map (fun (a,b) -> (a,1::b)) (getFreeVarsWI b p))
  | (LET (v,t,bb,e)) ->
    (List.map (fun (a,b) -> (a,1::b)) (getFreeVarsWI b bb)) @
    (List.map (fun (a,b) -> (a,2::b)) (getFreeVarsWI (b @ (getFVars [] v)) e))
  | (CASE (e,t,c)) ->
    (List.map (fun (a,b) -> (a,0::b)) (getFreeVarsWI b e)) @
    (let (_,x) = (List.fold_left (fun (n,r) -> (fun (p,e) -> (n+2,
                  (List.map (fun (a,b) -> (a,n::b)) (getFreeVarsWI (b @ (getFVars [] p)) e))@r)))
                  (2,[]) c)
     in
         x
     )
  | (NORMAL (e)) -> getFreeVarsWI b e
  | (REF x) -> getFreeVarsWI b (ExpIntern.decode_exp (REF x))
  | (_) -> [] ;;

let getFreeVarsWithIndices x = getFreeVarsWI [] x ;;

let rec getAVars e = match e with
  | (VAR v) -> [v]
  | (APPL(s,l)) ->
    List.fold_right List.append (List.map getAVars l) []
  | (QUANT (q,bb,e,p)) ->
    (getAVars e) @ (getAVars p) @ (strip bb)
  | (LET (v,t,bb,e)) ->
    (getAVars v) @ (getAVars bb) @ (getAVars e)
  | (CASE (e,t,c)) ->
    (getAVars e) @ (List.fold_left List.append [] (List.map (fun (p,e) -> (getAVars p) @ (getAVars e)) c))
  | (NORMAL (e)) -> (getAVars e)
  | (REF x) -> getAVars (ExpIntern.decode_exp (REF x))
  | _ -> [] ;;

let getAllVars e = Mylist.remove_dups (getAVars e) ;;

(*
 * Variable marking
 *)
let rec getMVars e = match e with
  | (APPL (_,l)) -> List.fold_right List.append (List.map getMVars l) []
  | (QUANT (q,b,e,p)) -> List.append (getMVars e) (getMVars p)
  | (LET (v,t,e,b)) -> List.append (getMVars e) (getMVars b)
  | (CASE (e,t,c)) ->
    (getMVars e) @ (List.fold_left List.append [] (List.map (fun (p,e) -> (getMVars e)) c))
  | (MARKED_VAR v) -> [v]
  | (NORMAL e) -> getMVars e
  | _ -> [] ;;

let getMarkedVars e = Mylist.remove_dups (getMVars e) ;;

let rec mVars b e = match e with
  | (VAR v) -> if List.mem v b then (VAR v)
                        else (MARKED_VAR v)
  | (APPL (v,l)) -> (APPL (v,List.map (fun x -> mVars b x) l))
  | (QUANT (q,bb,e,p)) ->
    (QUANT (q,bb,mVars (List.append b (strip bb)) e,
                   mVars (List.append b (strip bb)) p))
  | (LET (v,t,e,bb)) ->
    (LET (v,t,mVars b e,mVars (List.append b (getFreeVars v)) bb))
  | (CASE (e,t,c)) ->
    (CASE (mVars b e,t,
           List.map (fun (p,e) -> (p,mVars (List.append b (getFreeVars p)) e)) c))
  | (NORMAL (e)) -> (NORMAL (mVars b e))
  | (REF x) -> mVars b (ExpIntern.decode_exp (REF x))
  | x -> x ;;

let markVars x = mVars [] x ;;

let rec mVarsList b e l = match e with
  | (VAR v) -> if (List.mem v b) || not(List.mem v l)
                              then (VAR v)
                              else (MARKED_VAR v)
  | (APPL (v,ll)) ->
    (APPL (v,List.map (fun x -> mVarsList b x l) ll))
  | (QUANT (q,bb,e,p)) ->
    (QUANT (q,bb,mVarsList (List.append b (strip bb)) e l,
                   mVarsList (List.append b (strip bb)) p l))
  | (LET (v,t,e,bb)) ->
    (LET (v,t,mVarsList b e l,mVarsList (List.append b (getFreeVars v)) bb l))
  | (CASE (e,t,c)) ->
    (CASE (mVarsList b e l,t,
           List.map (fun (p,e) -> (p,mVarsList (List.append b (getFreeVars p)) e l)) c))
  | (NORMAL (e)) ->
    (NORMAL (mVarsList b e l))
  | (REF x) -> mVarsList b (ExpIntern.decode_exp (REF x)) l
  | x -> x ;;

let markVarsList x l = mVarsList  [] x l ;;

let rec unmarkVars e = match e with
  | (MARKED_VAR v) -> VAR v
  | (APPL (v,l)) -> APPL (v, List.map unmarkVars l)
  | (QUANT (v,b,e,p)) -> QUANT (v,b,unmarkVars e,unmarkVars p)
  | (LET (v,t,e,b)) -> LET (v,t,unmarkVars e,unmarkVars b)
  | (CASE (e,t,c)) ->
    (CASE (unmarkVars e,t, List.map (fun (p,e) -> (p,unmarkVars e)) c))
  | (NORMAL (e)) -> NORMAL (unmarkVars e)
  | x -> x ;;

let rec unmarkVarsList e l = match e with
  | (MARKED_VAR v) -> if (List.mem v l) then (VAR v) else (MARKED_VAR v)
  | (APPL (v,l2)) ->
    APPL (v, List.map (fun (x) -> unmarkVarsList x l) l2)
  | (QUANT (v,b,e,p)) ->
    QUANT (v,b,unmarkVarsList e l,unmarkVarsList p l)
  | (LET (v,t,e,b)) ->
    LET (v,t,unmarkVarsList e l,unmarkVarsList b l)
  | (CASE (e,t,c)) ->
    (CASE (unmarkVarsList e l,t, List.map (fun (p,e) -> (p,unmarkVarsList e l)) c))
  | (NORMAL (e)) -> NORMAL (unmarkVarsList e l)
  | (REF x) -> unmarkVarsList (ExpIntern.decode_exp (REF x)) l
  | x -> x ;;

let rec unmarkUnnormalVars e = match e with
  | (MARKED_VAR v) -> VAR v
  | (APPL (v,l)) -> APPL (v, List.map unmarkUnnormalVars l)
  | (QUANT (v,b,e,p)) ->
    QUANT (v,b,unmarkUnnormalVars e,unmarkVars p)
  | (LET (v,t,e,b)) -> LET (v,t,unmarkUnnormalVars e,unmarkVars b)
  | (CASE (e,t,c)) ->
    (CASE (unmarkUnnormalVars e,t, List.map (fun (p,e) -> (p,unmarkUnnormalVars e)) c))
  | (REF x) -> unmarkUnnormalVars (ExpIntern.decode_exp (REF x))
  | x -> x ;;

let rec findName v l = match l with
  | [] -> 0
  | ((a,b)::r) -> if v=a then b else findName v r

let rec unmarkVarAndName sl e = match e with
  | (MARKED_VAR v) ->
    let x = findName v sl
    in
        if x=0 then
            MARKED_VAR v
        else
            VAR x
  | (APPL (v,l)) -> APPL (v, List.map (unmarkVarAndName sl) l)
  | (QUANT (v,b,e,p)) ->
    QUANT (v,b,unmarkVarAndName sl e,unmarkVarAndName sl p)
  | (LET (v,t,e,b)) ->
    LET (v,t,unmarkVarAndName sl e,unmarkVarAndName sl b)
  | (CASE (e,t,c)) ->
    (CASE (unmarkVarAndName sl e,t, List.map (fun (p,e) -> (p,unmarkVarAndName sl e)) c))
  | (NORMAL (e)) -> NORMAL (unmarkVarAndName sl e)
  | (REF x) -> unmarkVarAndName sl (ExpIntern.decode_exp (REF x))
  | x -> x ;;

let rec unmarkVar s e = match e with
  | (MARKED_VAR v) -> if s=v then VAR v else MARKED_VAR v
  | (APPL (v,l)) -> APPL (v, List.map (unmarkVar s) l)
  | (QUANT (v,b,e,p)) -> QUANT (v,b,unmarkVar s e,unmarkVar s p)
  | (LET (v,t,e,b)) -> LET (v,t,unmarkVar s e,unmarkVar s b)
  | (CASE (e,t,c)) ->
    (CASE (unmarkVar s e,t, List.map (fun (p,e) -> (p,unmarkVar s e)) c))
  | (NORMAL (e)) -> NORMAL (unmarkVar s e)
  | (REF x) -> unmarkVar s (ExpIntern.decode_exp (REF x))
  | x -> x ;;

(*
 * Bound variable context
 *)
exception NotBound ;;
type context = C of string list list ;;
let emptyContext = C [] ;;
let rec depth s c = match c with
  | (C []) -> raise NotBound
  | (C (a::b)) ->
    if List.mem s a then 0 else 1 + (depth s (C b)) ;;

let addContext (C l) s = C (s::l) ;;
(*
 * Control abstractions
 *)
(*        fun mapFreeVar: (string * context -> Exp) -> Exp -> Exp
fun foldFreeVar: (('a * string * context) -> 'a) -> 'a -> Exp -> 'a*)

let rec try_names l s n =
    let s2 = Intern.intern (s ^ (string_of_int n)) in
        if List.mem s2 l then
            try_names l s (n + 1)
        else
            s2
    ;;

let name_away_from_list l s =
    if List.mem s l then
        try_names l (Intern.decode s) 0
    else
        s ;;

let name_away e s = name_away_from_list (getFreeVars e) s ;;

(*
 * Indexed variable stuff
 *)
type indexedVariable = int * (int list) ;;

let getIndexedVariableName (s,_) = s ;;

let isFreeIndexedVariable i = match i with
  | (s,[-1]) -> true
  | _ -> false ;;


exception FreeIndexedVariable ;;

let bindingIndex i = match i with
  | (s,[-1]) -> raise FreeIndexedVariable
  | (s,l) -> l ;;

let rec getBoundIndexedVars e = match e with
  | (APPL (f,l)) ->
    let (a,b) = List.fold_left (fun (r,n) -> (fun e ->
                    ((List.map (fun (s,l) -> (s,n::l))
                        (getBoundIndexedVars e)) @ r,
                        n + 1)
                    )) ([],0) l
    in a
  | (QUANT (q,v,e,p)) ->
    (List.map (fun (v,t) -> (v,[])) v) @
    (List.map (fun (v,l) -> (v,0::l)) (getBoundIndexedVars e)) @
    (List.map (fun (v,l) -> (v,1::l)) (getBoundIndexedVars p))
  | (LET (v,t,e,p)) ->
    (List.map (fun (v) -> (v,[])) (getFreeVars v)) @
    (List.map (fun (v,l) -> (v,1::l)) (getBoundIndexedVars e)) @
    (List.map (fun (v,l) -> (v,2::l)) (getBoundIndexedVars p))
  | (CASE (e,t,c)) ->
    (List.map (fun (v,l) -> (v,0::l)) (getBoundIndexedVars e)) @
    (let (_,x) = (List.fold_left
         (fun (n,r) -> (fun (p,e) ->
             (n+2,(List.map (fun (v,l) -> (v,n::l)) (getBoundIndexedVars e))@r)))
         (2,[]) c) in x) @
    (List.map (fun (x) -> (x,[])) (List.fold_left List.append [] (List.map (fun (p,e) -> getFreeVars p) c)))
  | _ -> [] ;;

let allIndexedVariables e =
    (List.map (fun (x) -> (x,[-1])) (getFreeVars e)) @
    (getBoundIndexedVars e) ;;

let allIndexedAndMarkedVariables e =
    (List.map (fun (x) -> (x,[-1])) (getFreeVars e)) @
    (List.map (fun (x) -> (Intern.intern ("_" ^ (Intern.decode x)),[-1])) (getMarkedVars e)) @
    (getBoundIndexedVars e) ;;

exception NotPrefix ;;
let rec delete_prefix l x = match (l,x) with
  | ([],x) -> x
  | (x,[]) -> [-1]
  | ((a::b),(c::d)) ->
    if a=c then delete_prefix b d else raise NotPrefix
  | (_,_) -> raise NotPrefix ;;

let rec toSubterm x l = match l with
  | ((v,[-1])::r) -> ((v,[-1])::(toSubterm x r))
  | ((v,l)::r) ->
    (try ((v,delete_prefix x l)::(toSubterm x r)) with NotPrefix -> (toSubterm x r)) ;;

let rec cut_last l = match l with
  | [x] -> []
  | (a::b) -> a::(cut_last b) ;;

let rec findIndexVariable ee e i = match (ee,i) with
  | ((VAR x),[]) -> (x,[-1])
  | ((VAR x),i) ->
     match (getSubterm e i) with
      | (LET (v,_,_,_)) -> (if List.mem x (getFreeVars v)  then
                                  (x,i)
                              else
                                  (findIndexVariable (VAR x) e (cut_last i)))
      | (QUANT (_,v,_,_)) -> (if List.mem x (strip v) then
                                    (x,i)
                                else
                                    (findIndexVariable (VAR x) e (cut_last i)))
      | _ -> findIndexVariable (VAR x) e (cut_last i) ;;

let whichIndexedVariable e i = findIndexVariable (getSubterm e i) e i ;

