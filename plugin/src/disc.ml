(******************************************************************************
 *
 * REWRITELIB
 *
 * disc.ml
 *
 * This file contains the implementation for discrimination nets
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

(* require "disc-s.sml" ;  *)
(* require "list.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "basis.__array" ;  *)

(* open listimpl ;*)
(* open Exp ;*)
(* open INTERNimpl ;*)
(* open EXP_INTERNimpl ;*)

(* infix 2 *| ;*)
(* infix 2 <| ;*)
(* infix 2 |> ;*)
(* infix 2 <> ;*)
(* infix 3 >< ;*)

open Exp ;;

type item = Var | Appl of int | Quant of int | Count of int |
            Other ;;
type trace = T of item | S of (item list) ;;
type discI = D of trace * (discI list)
           | E of exp list ;;
type disc = discI list ;;

type smallDisc = Exp.exp list array ;;

type ('a,'b) listDiscI = LT of 'a * (('a,'b) listDiscI list)
                       | LR of 'b ;;
type ('a,'b) listDisc = ('a,'b) listDiscI list ;;

type 'a expDisc = (item,(Exp.exp * 'a)) listDisc ;;

let small_size = 31 ;;

let smallHash e = match e with
  | (APPL (f,l)) -> f mod small_size
  | (QUANT (q,a,b,c)) -> q mod small_size
  | (VAR x) -> x mod small_size
  | (MARKED_VAR x) -> x mod small_size
  | x -> 0 ;;

let hashLeft e = match e with
  | (APPL (f,[l;r;c])) -> smallHash l
  | x -> smallHash x ;;

let newSmall = Array.init small_size (fun (x) -> ([])) ;;

let addSmall d el =
    let add = Array.init small_size (fun (x) ->(Array.get d x)) in
    let x = List.map (fun (x) -> Array.set add (hashLeft (ExpIntern.decode_exp (REF x))) ((REF x)::(Array.get add (hashLeft (ExpIntern.decode_exp (REF x)))))) el in
        add ;;

let findSmall d e = Array.get d (smallHash e) ;;

let rec all_exps l = match l with
  | [] -> [] 
  | (a::b) -> (all_exps_disci a)@(all_exps b)
and all_exps_disci d = match d with
  | (D(t,d)) -> all_exps d
  | (E(el)) -> el
  ;;

let allExps d = Mylist.remove_dups (all_exps d) ;;

let rec mkItem e = match e with
  | (VAR _) -> Var
  | (MARKED_VAR _) -> Var
  | (APPL (x,l)) -> (Appl x)
  | (QUANT (s,_,_,_)) -> (Quant s)
  | (NORMAL x) -> mkItem x
  | _ -> Other
  ;;

let rec mkSetTrace e = match e with
  | ((VAR x)::r) -> mkSetTrace r
  | ((MARKED_VAR x)::r) -> mkSetTrace r
  | (x::r) -> mkItem x::mkSetTrace r
  | [] -> [] ;;

let mkTrace ac e = match e with
  | (APPL (x,l)) ->
    if ac x then
        [T(Appl x);S(Mylist.remove_dups(mkSetTrace l))]
    else
        T(Appl x)::(List.map (fun (x) -> T(x)) (List.map mkItem l))
  | (QUANT(s,v,e,p)) -> [T(Quant s);T(mkItem e);T(mkItem p)]
  | x -> [T(mkItem x)] ;;

let mkITrace e = match e with
  | (APPL (x,l)) -> (Appl x)::(List.map mkItem l)
  | (QUANT(s,v,e,p)) -> [(Quant s);(mkItem e);(mkItem p)]
  | x -> [mkItem x] ;;

let mkDTrace x = (Count (List.length (allSubterms x)))::(mkITrace x) ;;

let newItem = [] ;;

let rec addItem l1 l2 x = match (l1,l2) with
  | ([],[]) -> [LR x]
  | ((a::b),[]) -> a::(addItem b [] x)
  | (((LR v)::b),(f::r)) -> (LR v)::(addItem b (f::r) x)
  | (((LT (a,d))::b),(f::r)) ->
    if a=f then
        (LT (a,addItem d r x))::b
    else
        (LT (a,d))::(addItem b (f::r) x)
  | ([],(a::b)) -> [LT (a,addItem [] b x)] ;;

let rec findItem l1 l2 = match (l1,l2) with
  | ([],_) -> []
  | (((LR v)::r),[]) -> v::(findItem r [])
  | (((LT (a,d))::r),[]) -> findItem r []
  | (((LR v)::b),(f::r)) -> findItem b (f::r)
  | (((LT (a,d))::b),(f::r)) ->
    if a=f then
        findItem d r
    else
        findItem b (f::r) ;;

let newExpItem = [] ;;

let addExpItem d e i =
    addItem d (mkDTrace e) (e,i) ;;

let findExpItem d exp =
    (List.map (fun (e,i) -> i) (List.filter (fun (e,i) -> e=exp) (findItem d (mkDTrace exp)))) ;;

let makeistring e = match e with
  | Var -> "Var"
  | (Appl s) -> (Intern.decode s)
  | (Quant s) -> "Q(" ^ (Intern.decode s) ^ ")"
  | Other -> "Other"
  ;;

let rec foldstring m l = match l with
  | [] -> ""
  | (a::b::c) -> (m a) ^ " " ^ (foldstring m (b::c))
  | [a] -> (m a)
  ;;

let maketstring d = match d with
  | (T i) -> "T{"^ (makeistring i) ^ "}"
  | (S il) -> "S{" ^ (foldstring makeistring il) ^ "}"
  ;;

let rec maketlstring l = match l with
  | [] -> ""
  | (a::b) -> (maketstring a) ^ (maketlstring b)
  ;;

let rec makedstring d = match d with
  | (D(t,l)) -> "D" ^ (maketstring t) ^ "[" ^
               (foldstring makedstring l) ^ "]"
  | (E(l)) -> "E(" ^ foldstring (fun (x) -> (prExp (ExpIntern.decode_exp x))) l ^ ")"
  ;;

let rec makedcstring l = match l with
  | [] -> ""
  | (a::b) -> (makedstring a) ^ " " ^ (makedcstring b)
  ;;

let rec addExp e exp = match e with
  | [] -> [E([exp])]
  | (E(l)::r) -> E(exp::l)::r
  | (D(i,d)::r) -> D(i,d)::addExp r exp ;;

let rec addTrace disc l exp = match l with
  | [] -> addExp disc exp
  | (T(Var)::r) -> addAll disc r exp
  | (T(i)::r) -> addSingle disc (T(i)) r exp
  | [S(l)] -> addSet disc l exp
and addHVAll l r exp = match l with
  | [] -> []
  | (E(l)::rr) -> E(l)::addHVAll rr r exp
  | (D(i,d)::rr) -> D(i,addTrace d r exp)::addHVAll rr r exp
and addAll l r exp = match l with
  | [] -> [D(T(Var),addTrace [] r exp)]
  | (E(l)::rr) -> E(l)::addAll rr r exp
  | (D(T(Var),d)::rr) -> D(T(Var),addTrace d r exp)::addHVAll rr r exp
  | (D(i,d)::rr) -> D(i,addTrace d r exp)::addAll rr r exp
and addSingle l i r exp = match l with
  | [] -> [D(i,addTrace [] r exp)]
  | (E(l)::rr) -> E(l)::(addSingle rr i r exp)
  | (D(i2,d)::rr) ->
      if i = i2 then
          D(i2,addTrace d r exp)::rr
      else
          D(i2,d)::addSingle rr i r exp
and addSet ll l exp = match ll with
  | [] -> [D(S(l),[E([exp])])]
  | (E(l2)::r) -> E(l2)::(addSet r l exp)
  | (D(S(l1),r1)::r2) ->
    if Mylist.is_subset l1 l && Mylist.is_subset l l1 then
        D(S(l1),addExp r1 exp)::r2
    else
        D(S(l1),r1)::(addSet r2 l exp) ;;

let rec findInt a l = match l with
  | (T(Var)::r) -> varFind a r
  | [] -> endFind a
  | (T(x)::r) -> normalFind a x r
  | [S(x)] -> setFind a x
and setFind l x = match l with
  | [] -> []
  | (D(T(v),r1)::r2) -> setFind r2 x
  | (D(S(v),r1)::r2) ->
    if (Mylist.is_subset v x) then
        (endFind r1)@(setFind r2 x)
    else
        setFind r2 x
  | (D(T(Var),r1)::r2) ->
    (endFind r1)@(setFind r2 x)
  | (E(e)::r) -> e@(setFind r x)
  | (D(_,r1)::r2) -> setFind r2 x
and normalFind l x r = match l with
  | [] -> []
  | (D(T(Var),l)::r2) -> (endFind l)@(findInt l r)@(normalFind r2 x r)
  | (E(_)::r2) -> normalFind r2 x r
  | (D(T(c),l)::r2) ->
    if x = c then
        (findInt l r)@(normalFind r2 x r)
    else
        normalFind r2 x r
and varFind l r = match l with
  | [] -> []
  | (D(T(Var),l)::r2) -> findInt l r
  | (f::r2) -> varFind r2 r
and endFind l = match l with
  | [] -> []
  | (D(_,_)::r) -> endFind r
  | (E(e)::_) -> e ;;

let rec ufindInt a l = match l with
  | (T(Var)::r) -> uvarFind a r
  | [] -> uendFind a
  | (T(x)::r) -> unormalFind a x r
  | [S(x)] -> usetFind a x
and usetFind l x = match l with
  | [] -> []
  | (D(T(v),r1)::r2) -> usetFind r2 x
  | (D(S(v),r1)::r2) ->
    if (Mylist.is_subset v x) then
        (uendFind r1)@(usetFind r2 x)
    else
        usetFind r2 x
  | (D(T(Var),r1)::r2) -> (uendFind r1)@(usetFind r2 x)
  | (E(e)::r) -> e@(usetFind r x)
  | (D(_,r1)::r2) -> usetFind r2 x
and unormalFind l x r = match l with
  | [] -> []
  | (D(T(Var),l)::r2) -> (uendFind l)@(ufindInt l r)@(unormalFind r2 x r)
  | (E(_)::r2) -> unormalFind r2 x r
  | (D(T(c),l)::r2) ->
    if x = c then
        (ufindInt l r)@(unormalFind r2 x r)
    else
        unormalFind r2 x r
and uvarFind l r = match l with
  | [] -> []
  | (D(_,l)::r2) -> ufindInt l r
  | (f::r2) -> uvarFind r2 r
and uendFind l = match l with
  | [] -> []
  | (D(_,_)::r) -> uendFind r
  | (E(e)::_) -> e
  ;;

let rec collect_rules l = match l with
  | (E(e)::r) -> e@(collect_rules r)
  | (D(t,l)::r) -> (collect_rules l)@(collect_rules r)
  | [] -> []
  ;;

let rec collect_symbol sym l = match l with
  | (D(T(Appl (s)),l)::r) ->
    if sym=s then
        collect_rules l
    else
        collect_symbol sym r
  | (_::r) -> collect_symbol sym r
  | [] -> []
  ;;

let newDisc = [] ;;

let rec add ac d e = match e with
  | (APPL (v,[l;r;c])) ->
    let t = ExpIntern.intern_exp ac (APPL (v,[l;r;c])) in
    let res = addTrace d (mkTrace ac l) (ExpIntern.intern_exp ac (APPL (v,[l;r;c])))
        (*val _ = trace "Disc" (fn (x) => ("Adding: " ^ (prExp (APPL (v,[l,r,c])))))
        val _ = trace "Disc" (fn (x) => ("Trace: " ^ (maketlstring (mkTrace ac (APPL (v,[l,r,c]))))))
        val _ = trace "Disc" (fn (x) => ("Old disc: " ^ (makedcstring d)))
        val _ = trace "Disc" (fn (x) => ("New disc: " ^ (makedcstring res)))*)
    in
        res
 | (REF x) -> add ac d (ExpIntern.decode_exp (REF x))
 | e -> print_string ("Disc.add: Cannot add " ^ (prExp e) ^ "\n");d
    ;;

let find ac a b = findInt a (mkTrace ac b) ;;

let ufind ac a b = ufindInt a (mkTrace ac b) ;;

let rec delete_exp l b = match l with
  | [] -> []
  | (D(T(Appl s),l)::r) ->
    if b=s then
        r
    else
        D(T(Appl s),l)::(delete_exp r b)
  | (D(T(v),l)::r) -> D(T(v),l)::(delete_exp r b)
  ;;

let makeAC sym t =
    List.fold_right (fun a -> (fun b -> add (fun (x) -> x=sym) b a))
          (Mylist.remove_dups (collect_symbol sym t))
          (delete_exp t sym)
  ;;





