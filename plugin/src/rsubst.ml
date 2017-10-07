(******************************************************************************
 *
 * REWRITELIB
 *
 * subst.ml
 *
 * Substitution implementation
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
(* require "context.sml" ;  *)
(* require "list.sml" ;  *)

open Exp ;;
(* open CONTEXTimpl ; *)
(* open listimpl ; *)
(* open EXP_INTERNimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

(*fun member x nil = false
  | member x (f::r) = x=f orelse member x r*)

type unifier = U of (int * exp) list ;;

(*
 * Constructor functions
 *)
let empty = U [] ;;
let addPair (U x) s e = (U ((s,e)::x)) ;;
let rec addNormals u = match u with
  | (U []) -> (U [])
  | (U ((s,e)::b)) ->
    (let (U bb) = addNormals (U b) in
        (U ((s,NORMAL e)::bb)))
  ;;
(*
 * Unifier masking
 *)
let rec ss lx l = match lx with
  | [] -> []
  | ((s,e)::r) ->
    if List.mem s l then
        (s,e)::(ss r l)
    else
        ss r l ;;
let select_subs (U x) l = (U (ss x l)) ;;

let rec rs ll l = match ll with
  | [] -> []
  | ((s,e)::r) ->
    if List.mem s l then
        rs r l
    else
       (s,e)::(rs r l) ;;

let remove_subs (U x) l = (U (rs x l)) ;;
let merge_subs (U x1) (U x2) = (U (x1@x2)) ;;

(*
 * Retrieval
 *)
let rec dom (U l) = match l with
  | [] -> []
  | ((a,x)::b) -> a::(dom (U b)) ;;

exception NoUnifier ;;

let rec apply (U l) s = match l with
  | [] -> raise NoUnifier
  | ((a,x)::r) -> if a = s then x else apply (U r) s ;;

let rec strip l = match l with
  | [] -> []
  | ((a,b)::r) -> a::(strip r) ;;

let rec bsubst b u e = match e with
  | (REF x) -> bsubst b u (ExpIntern.decode_exp (REF x))
  | (VAR v) ->
    (try (if List.mem v b then
        (VAR v)
    else
        apply u v) with NoUnifier -> (VAR v))
  | (APPL (n,l)) ->
    APPL (n,List.map (fun x -> bsubst b u x) l)
  | (QUANT (n,v,e,p)) ->
    let
        nb = List.append b (strip v)
    in
        (QUANT (n,v,bsubst nb u e,bsubst nb u p))
  | (CASE (e,t,c)) ->
    let
        c2 = List.map (fun (p,e) ->
                         let bbb = List.append b (Rcontext.getFreeVars p)
                         in
                              (p,bsubst bbb u e)
                     ) c
    in
        (CASE (bsubst b u e,t,c2))
  | (LET (v,t,e,bb)) ->
    let
        bbb = List.append b (Rcontext.getFreeVars v)
    in
        (LET (v,t,bsubst b u e,bsubst bbb u bb))
  | (CASE (e,t,c)) ->
    let bbb = List.append b (Rcontext.getFreeVars e) in
    let c2 = List.map (fun (p,e) ->
                         (p,
                          let bb = List.append b (Rcontext.getFreeVars p) in bsubst bb u e)
                     ) c
    in
        (CASE (bsubst b u e,t,c2))
  | (INDEX (e,s,i)) -> (INDEX ((bsubst b u e),s,i))
  | e -> e ;;
let subst u e = bsubst [] u e ;;

let compose (U x1) (U x2) =
    let x2 = List.map (fun (a,x) -> (a,subst (U x1) x)) x2 in
    let x1 = List.filter (fun (a,_) -> not(List.mem a (dom (U x2)))) x1
    in
        U (x1@x2)


