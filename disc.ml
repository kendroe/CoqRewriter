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
 * All rights reserved--This is an incomplete work.  An appropriate license
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

type item = Var | Appl of int | Quant of int | Count of int |
            Other ;;
type trace = T of Item | S of (Item list) ;;
type discI = D of Trace * (DiscI list)
           | E of Exp list ;;
type disc = DiscI list ;;

type smallDisc = Exp.exp list array

type ('a,'b) listDiscI = LT of ''a * (('a,'b) listDiscI list)
                       | LR of 'b
type ('a,'b) listDisc = ('a,'b) listDiscI list

type 'a expDisc = (item,(Exp.exp * 'a)) listDisc

val small_size = 31 ;

let smallHash e = match e with
  | (APPL (f,l)) -> f mod small_size
  | (QUANT (q,a,b,c)) -> q mod small_size
  | (VAR x) -> x mod small_size
  | (MARKED_VAR x) -> x mod small_size
  | x -> 0 ;;

let hashLeft e = match e with
  | (APPL (f,[l,r,c])) -> smallHash l
  | hashLeft x -> smallHash x ;;

let newSmall = Array.init small_size (fun (x) -> (nil : Exp.exp list)) ;;

let addSmall d el =
    let add = Array.tabulate small_size (fun (x) -> (Array.get d x : Exp.exp list)) in
    let x = app (fun (x) -> Array.update (add,(hashLeft (decode_exp (REF x))),((REF x)::(Array.sub (add,(hashLeft (decode_exp (REF x)))))))) el in
        add ;;

let findSmall d e = Array.get d smallHash e ;;

fun all_exps nil = nil
  | all_exps (a::b) = (all_exps_disci a)@(all_exps b)
and all_exps_disci (D(t,d)) = all_exps d
  | all_exps_disci (E(el)) = el

fun allExps d = remove_dups (all_exps d)

fun mkItem (VAR _) = Var
  | mkItem (MARKED_VAR _) = Var
  | mkItem (APPL (x,l)) = (Appl x)
  | mkItem (QUANT (s,_,_,_)) = (Quant s)
  | mkItem (NORMAL x) = mkItem x
  | mkItem _ = Other
  ;

fun mkSetTrace ((VAR x)::r) = mkSetTrace r
  | mkSetTrace ((MARKED_VAR x)::r) = mkSetTrace r
  | mkSetTrace (x::r) = mkItem x::mkSetTrace r
  | mkSetTrace nil = nil ;

fun mkTrace ac (APPL (x,l)) =
    if ac x then
        [T(Appl x),S(remove_dups(mkSetTrace l))]
    else
        T(Appl x)::(map (fn (x) => T(x)) (map mkItem l))
  | mkTrace ac (QUANT(s,v,e,p)) = [T(Quant s),T(mkItem e),T(mkItem p)]
  | mkTrace ac x = [T(mkItem x)] ;

fun mkITrace (APPL (x,l)) =
    Appl x::(map mkItem l)
  | mkITrace (QUANT(s,v,e,p)) = [(Quant s),(mkItem e),(mkItem p)]
  | mkITrace x = [mkItem x] ;

fun mkDTrace x = (Count (length (allSubterms x)))::(mkITrace x)

val newItem = nil

fun addItem nil nil x = [LR x]
  | addItem (a::b) nil x = a::(addItem b nil x)
  | addItem ((LR v)::b) (f::r) x = (LR v)::(addItem b (f::r) x)
  | addItem ((LT (a,d))::b) (f::r) x =
    if a=f then
        (LT (a,addItem d r x))::b
    else
        (LT (a,d))::(addItem b (f::r) x)
  | addItem nil (a::b) x = [LT (a,addItem nil b x)]

fun findItem nil _ = nil
  | findItem ((LR v)::r) nil = v::(findItem r nil)
  | findItem ((LT (a,d))::r) nil = findItem r nil
  | findItem ((LR v)::b) (f::r) = findItem b (f::r)
  | findItem ((LT (a,d))::b) (f::r) =
    if a=f then
        findItem d r
    else
        findItem b (f::r)

val newExpItem = nil

fun addExpItem d e i =
    addItem d (mkDTrace e) (e,i) ;

fun findExpItem d exp =
    findItem d (mkDTrace exp) |> (fn (e,i) => e=exp) <> (fn (e,i) => i)

fun makeistring Var = "Var"
  | makeistring (Appl s) = (decode s)
  | makeistring (Quant s) = "Q(" ^ (decode s) ^ ")"
  | makeistring Other = "Other"
  ;

fun foldstring m nil = ""
  | foldstring m (a::b::c) = (m a) ^ " " ^ (foldstring m (b::c))
  | foldstring m [a] = (m a)
  ;

fun maketstring (T i) = "T{"^ (makeistring i) ^ "}"
  | maketstring (S il) = "S{" ^ (foldstring makeistring il) ^ "}"
  ;

fun maketlstring nil = ""
  | maketlstring (a::b) = (maketstring a) ^ (maketlstring b)
  ;

fun makedstring (D(t,l)) = "D" ^ (maketstring t) ^ "[" ^
               (foldstring makedstring l) ^ "]"
  | makedstring (E(l)) = "E(" ^ foldstring prExp l ^ ")"
  ;
fun makedcstring nil = ""
  | makedcstring (a::b) = (makedstring a) ^ " " ^ (makedcstring b)
  ;

fun addExp nil exp = [E([exp])]
  | addExp (E(l)::r) exp = E(exp::l)::r
  | addExp (D(i,d)::r) exp = D(i,d)::addExp r exp ;

fun addTrace disc nil exp = addExp disc exp
  | addTrace disc (T(Var)::r) exp = addAll disc r exp
  | addTrace disc (T(i)::r) exp = addSingle disc (T(i)) r exp
  | addTrace disc [S(l)] exp = addSet disc l exp
and addHVAll nil r exp = nil
  | addHVAll (E(l)::r) x exp = E(l)::addHVAll r x exp
  | addHVAll (D(i,d)::r) x exp =
        D(i,addTrace d x exp)::addHVAll r x exp
and addAll nil r exp = [D(T(Var),addTrace nil r exp)]
  | addAll (E(l)::r) x exp = E(l)::addAll r x exp
  | addAll (D(T(Var),d)::r) x exp =
        D(T(Var),addTrace d x exp)::addHVAll r x exp
  | addAll (D(i,d)::r) x exp = D(i,addTrace d x exp)::addAll r x exp
and addSingle nil i r exp = [D(i,addTrace nil r exp)]
  | addSingle (E(l)::r) i x exp = E(l)::(addSingle r i x exp)
  | addSingle (D(i,d)::r) i2 x exp =
      if i = i2 then
          D(i,addTrace d x exp)::r
      else
          D(i,d)::addSingle r i2 x exp
and addSet nil l exp =
    [D(S(l),[E([exp])])]
  | addSet (E(l)::r) l2 exp =
    E(l)::(addSet r l2 exp)
  | addSet (D(S(l1),r1)::r2) l2 exp =
    if is_subset l1 l2 andalso is_subset l2 l1 then
        D(S(l1),addExp r1 exp)::r2
    else
        D(S(l1),r1)::(addSet r2 l2 exp) ;

fun findInt a (T(Var)::r) = varFind a r
  | findInt a nil = endFind a
  | findInt a (T(x)::r) = normalFind a x r
  | findInt a [S(x)] = setFind a x
and setFind nil x = nil
  | setFind (D(T(v),r1)::r2) x = setFind r2 x
  | setFind (D(S(v),r1)::r2) x =
    if (is_subset v x) then
        (endFind r1)@(setFind r2 x)
    else
        setFind r2 x
  | setFind (D(T(Var),r1)::r2) x =
    (endFind r1)@(setFind r2 x)
  | setFind (E(e)::r) x = e@(setFind r x)
  | setFind (D(_,r1)::r2) x = setFind r2 x
and normalFind nil x r = nil
  | normalFind (D(T(Var),l)::r) x r2 = (endFind l)@(findInt l r2)@(normalFind r x r2)
  | normalFind (E(_)::r) x r2 = normalFind r x r2
  | normalFind (D(T(c),l)::r) x r2 =
    if x = c then
        (findInt l r2)@(normalFind r x r2)
    else
        normalFind r x r2
and varFind nil r = nil
  | varFind (D(T(Var),l)::r) r2 = findInt l r2
  | varFind (f::r) r2 = varFind r r2
and endFind nil = nil
  | endFind (D(_,_)::r) = endFind r
  | endFind (E(e)::_) = e ;

fun ufindInt a (T(Var)::r) = uvarFind a r
  | ufindInt a nil = uendFind a
  | ufindInt a (T(x)::r) = unormalFind a x r
  | ufindInt a [S(x)] = usetFind a x
and usetFind nil x = nil
  | usetFind (D(T(v),r1)::r2) x = usetFind r2 x
  | usetFind (D(S(v),r1)::r2) x =
    if (is_subset v x) then
        (uendFind r1)@(usetFind r2 x)
    else
        usetFind r2 x
  | usetFind (D(T(Var),r1)::r2) x =
    (uendFind r1)@(usetFind r2 x)
  | usetFind (E(e)::r) x = e@(usetFind r x)
  | usetFind (D(_,r1)::r2) x = usetFind r2 x
and unormalFind nil x r = nil
  | unormalFind (D(T(Var),l)::r) x r2 = (uendFind l)@(ufindInt l r2)@(unormalFind r x r2)
  | unormalFind (E(_)::r) x r2 = unormalFind r x r2
  | unormalFind (D(T(c),l)::r) x r2 =
    if x = c then
        (ufindInt l r2)@(unormalFind r x r2)
    else
        unormalFind r x r2
and uvarFind nil r = nil
  | uvarFind (D(_,l)::r) r2 = ufindInt l r2
  | uvarFind (f::r) r2 = uvarFind r r2
and uendFind nil = nil
  | uendFind (D(_,_)::r) = uendFind r
  | uendFind (E(e)::_) = e ;

fun collect_rules (E(e)::r) = e@(collect_rules r)
  | collect_rules (D(t,l)::r) = (collect_rules l)@(collect_rules r)
  | collect_rules nil = nil
  ;

fun collect_symbol sym (D(T(Appl (s)),l)::r) =
    if sym=s then
        collect_rules l
    else
        collect_symbol sym r
  | collect_symbol sym (_::r) = collect_symbol sym r
  | collect_symbol sym nil = nil
  ;

val new = nil ;
fun add ac d (APPL (v,[l,r,c])) =
    let val res = addTrace d (mkTrace ac l) (intern_exp ac (APPL (v,[l,r,c]))) ;
        (*val _ = trace "Disc" (fn (x) => ("Adding: " ^ (prExp (APPL (v,[l,r,c])))))
        val _ = trace "Disc" (fn (x) => ("Trace: " ^ (maketlstring (mkTrace ac (APPL (v,[l,r,c]))))))
        val _ = trace "Disc" (fn (x) => ("Old disc: " ^ (makedcstring d)))
        val _ = trace "Disc" (fn (x) => ("New disc: " ^ (makedcstring res)))*)
    in
        res
    end

fun find ac a b = findInt a (mkTrace ac b) ;

fun ufind ac a b = ufindInt a (mkTrace ac b)

fun delete_exp nil b = nil
  | delete_exp (D(T(Appl s),l)::r) b =
    if b=s then
        r
    else
        D(T(Appl s),l)::(delete_exp r b)
  | delete_exp (D(T(v),l)::r) b = D(T(v),l)::(delete_exp r b)
  ;

fun makeAC sym t =
    foldr (fn (a,b) => add (fn (x)=>x=sym) b a)
          (delete_exp t sym)
          (remove_dups (collect_symbol sym t))
  ;





