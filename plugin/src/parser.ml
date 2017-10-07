(*****************************************************************************
 *
 * REWRITELIB
 *
 * parser.ml
 * 
 * This file contains the implementation for the parser
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

(* require "lex.sml" ;  *)
(* require "lex-s.sml" ;  *)
(* require "list.sml" ;  *)
(* require "parser-s.sml" ;  *)

(* infix 3 |||| ; *)
(* infix 4 ||| ; *)
(* infix 5 ==> ; *)
(* infix 6 & ; *)

(* open listimpl ; *)

(* structure L = L ; *)
(* open L ; *)

open Lex;;

let rec strip l = match l with
  | [] -> []
  | ((t,f,a,b,c,d)::r) -> t::(strip r) ;;

let rec getTokenRc tp t = match (tp,t) with
  | ((NUMBER x),((NUMBER y,f,a,b,c,d)::r)) ->
      [(r,(NUMBER y),f,a,b,c,d)]
  | ((RAT (x,y)),((RAT (xx,yy),f,a,b,c,d)::r)) -> 
      [(r,(RAT (xx,yy)),f,a,b,c,d)]
  | ((ID x),((ID y,f,a,b,c,d)::r)) ->
    if x = "" || x = y then [(r,(ID y),f,a,b,c,d)] else
        raise (Failure (((ID y)::(strip r))))
  | ((C_QUOTE x),((C_QUOTE y,f,a,b,c,d)::r)) ->
    if x = '\000' || x = y then [(r,(C_QUOTE y),f,a,b,c,d)] else
        raise (Failure ((C_QUOTE y)::(strip r)))
  | ((QUOTE x),((QUOTE y,f,a,b,c,d)::r)) ->
    if x = "" || x = y then [(r,(QUOTE y),f,a,b,c,d)] else
        raise (Failure ((QUOTE y)::(strip r)))
  | ((SYMBOL x),((SYMBOL y,f,a,b,c,d)::r)) ->
      if (x = "") || x = y then [(r,(SYMBOL y),f,a,b,c,d)]
                  else raise (Failure ((SYMBOL y)::(strip r)))
  | (tok,((t,f,a,b,c,d)::r)) ->
    if t = tok then [(r,t,f,a,b,c,d)] else raise (Failure (t::(strip r)))
  | (_,l) -> raise (Failure (strip l)) ;;

let rec getToken tok list =
    let l = getTokenRc tok (List.map (fun (a) -> (a,"",0,0,0,0)) list)
    in
        List.map (fun (r,t,f,a,b,c,d) -> (strip r,t)) l ;;

let rec ( & ) f1 f2 l =
    List.fold_right List.append
         (List.map (fun (l,v1) -> (List.map (fun (l,v2) -> (l,(v1,v2))) (f2 l)))
              (f1 l)) [] ;;

let ( ||| ) f1 f2 l = try f1 l with Failure (e) -> f2 l ;;

let ( |||| ) f1 f2 l =
    try
        let (v1,_) = (f1 l,[]) in
        let (v2,f) = (f2 l,[]) in
        let r = List.append v1 v2 in
            if r = [] then raise (Failure (f)) else [List.hd r]
    with Failure e -> []
    ;;

let ( ==> ) f m = (fun l ->
    List.map (fun (l,v) -> (l,m v)) (f l)) ;;

type precedence = LEFT of token * int | RIGHT of token * int ;;

let rec popStack s tcombine v = match s with
  | [] -> v
  | ((t,oper)::r) -> popStack r tcombine (tcombine t oper v) ;;

let rec getPrec pl oper = match pl with
  | ((LEFT (t,i))::r) -> if t = oper then (LEFT (t,i)) else
                         getPrec r oper
  | ((RIGHT (t,i))::r) -> if t = oper then (RIGHT (t,i)) else
                          getPrec r oper ;;

let smaller pl op1 op2 =
    let v1 = getPrec pl op1 in
    let v2 = getPrec pl op2 in
        (match (v1,v2) with
          | (RIGHT (_,i),LEFT (_,j))   -> i <= j
          | (RIGHT (_,i),RIGHT (_,j))  -> i <= j
          | (LEFT (_,i),LEFT (_,j))    -> i < j
          | (LEFT (_,i),RIGHT (_,j))   -> i < j
        ) ;;

let rec validOp pl oper = match pl with
  | [] -> false
  | ((LEFT (t,i))::r) -> if t = oper then true else
                         validOp r oper
  | ((RIGHT (t,i))::r) -> if t = oper then true else
                          validOp r oper ;;

let rec updateStack r pl tcombine v2 op2 = match r with
  | [] -> [(v2,op2)]
  | ((v1,op1)::r) ->
      if smaller pl op1 op2 then
          (v2,op2)::(v1,op1)::r
      else
          updateStack r pl tcombine (tcombine v1 op1 v2) op2 ;;

let rec pParse stack pl tparse tcombine tokens =
    (match tparse tokens with
         ((oper::t,v)::_) -> if validOp pl oper then
                 pParse (updateStack stack pl tcombine v oper) pl tparse
                 tcombine t
             (*else if validOp pl (SYMBOL " ") then
                 (pParse (updateStack stack pl tcombine v (SYMBOL " ")) pl tparse
                  tcombine (oper::t) handle Failure _ => [(oper::t,popStack stack tcombine v)])*)
             else
                 [(oper::t,popStack stack tcombine v)]
           | (([],v)::_) -> [([],popStack stack tcombine v)]
    );;

let precParse pl tparse tcombine tokens =
    pParse [] pl tparse tcombine tokens ;;


