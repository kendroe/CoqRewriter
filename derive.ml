(******************************************************************************
 *        
 * REWRITELIB                        
 *                                   
 * derive.ml                       
 *                               
 * This file contains the code for deriving new rewrite rules from existing
 * rules.
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
(* require "env.sml" ;  *)
(* require "derive-s.sml" ;  *)
(* require "context.sml" ;  *)
(* require "type.sml" ;  *)
(* require "match.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "disc.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "trace.sml" ;  *)
(* require "cache.sml" ;  *)
(* require "intern.sml" ;  *)

open Exp ;;
open Intern ;;

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

(* open ENVimpl ; *)
(* open INTERNimpl ; *)
(* open CONTEXTimpl ; *)
(* open TYPEimpl ; *)
(* open MATCHimpl ; *)
(* open EXP_INTERNimpl ; *)
(* open DISCimpl ; *)
(* open SUBSTimpl ; *)
(* open TRACEimpl ; *)
(* open CACHEimpl ; *)

let rec add_types l = match l with
  | [] -> []
  | (a::b) -> (a,Type.notype)::(add_types b)
  ;;

let rec mk_and_conds c s l l2 = match l2 with
  | [] -> []
  | (a::b) ->
    let cond = (APPL (intern_and,c::(l@b))) in
    let vars = Mylist.difference (Context.getFreeVars cond) (Context.getFreeVars a) in
    let cond2 = if vars=[] then cond else
                (QUANT (intern_exists,(add_types vars),cond,(APPL (intern_true,[]))))
    in
        (APPL (s,[a;(APPL (intern_false,[]));cond2]))::
            (mk_and_conds c s (a::l) b)
    ;;

let rec mk_or_conds c s l l2 = match l2 with
  | [] -> []
  | (a::b) ->
    let cond = (APPL (intern_and,c::(List.map (fun (x) -> (APPL (intern_not,[x]))) (l@b)))) in
    let vars = Mylist.difference (Context.getFreeVars cond) (Context.getFreeVars a) in
    let cond2 = if vars=[] then cond else
                    (QUANT (Intern.intern_exists,(add_types vars),cond,(APPL (intern_true,[]))))
    in
        (APPL (s,[a;(APPL (intern_true,[]));cond2]))::
        (mk_or_conds c s (a::l) b)
  ;;

let is_constructor_term f = (decode f) >= "A" && (decode f) <= "ZZZZ"

let rec is_constant e = match e with
  | (APPL (f,l)) ->
    is_constructor_term f && is_constant_list l
  | (NUM x) -> true
  | (STRING s) -> true
  | (CHAR s) -> true
  | _ -> false
and is_constant_list l = match l with
  | [] -> true
  | (a::b) -> is_constant a && is_constant_list b
  ;;

let is_default e = match e with
  | (APPL (f,l)) -> f=71 && f=72
  | _ -> false

let rec strip l = match l with
  | [] -> []
  | ((f,_)::r) -> f::(strip r)
  ;;

let false_test x y = false ;;

let equality_test = ref (false_test) ;;

let set_equality_test f = (equality_test := f) ;;

let equality_terms f g = ((!equality_test) (ExpIntern.decode_exp f) (ExpIntern.decode_exp g)) || 
                         ((is_constant f || is_default f) &&
                          not(is_constant g) && not(is_default g)) ;;

let rec derive_rules e = match e with
  | (APPL (s,[APPL(9,l);NORMAL (APPL(4,[]));c])) ->
    (List.map (fun (x) -> (APPL (s,[x;NORMAL (APPL (intern_true,[]));c]))) l,[])
  | (APPL (s,[APPL(10,l);NORMAL (APPL(5,[]));c])) ->
    ((List.map (fun (x) -> (APPL (s,[x;NORMAL (APPL (intern_false,[]));c]))) l),[])
  | (APPL (s,[APPL(9,l);(APPL(4,[]));c])) ->
    (List.map (fun (x) -> (APPL (s,[x;NORMAL (APPL (intern_true,[]));c]))) l,[])
  | (APPL (s,[APPL(10,l);(APPL(5,[]));c])) ->
    (List.map (fun (x) -> (APPL (s,[x;NORMAL (APPL (intern_false,[]));c]))) l,[])
  | (APPL (s,[APPL(17,[l]);r;c])) ->
    ([APPL (s,[l;(APPL (intern_not,[r]));c])],[])
  | (APPL (s,[APPL(18,[c;e;(APPL (5,[]))]);NORMAL (APPL (4,[]));cond])) ->
    ([APPL (s,[c;NORMAL (APPL (intern_true,[]));cond]);
      APPL (s,[e;NORMAL (APPL (intern_true,[]));cond])],[])
  | (APPL (s,[APPL(18,[c;e;(APPL (5,[]))]);(APPL (4,[]));cond])) ->
    ([APPL (s,[c;NORMAL (APPL (intern_true,[]));cond]);
      APPL (s,[e;NORMAL (APPL (intern_true,[]));cond])],[])
  | (APPL (s,[APPL(18,[c;e1;e2]);r;cond])) ->
    ([APPL (s,[e1;r;(APPL (intern_and,[c;cond]))]);
     APPL (s,[e2;r;(APPL (intern_and,[cond;APPL(intern_not,[c])]))])],[])
  | (APPL (s,[APPL(11,[a;b]);
                           NORMAL (APPL(4,[]));
                           c])) ->
      ((if is_constant a || is_default a then
          (if is_constant b || is_default b then
               [APPL (intern_unoriented_rule,[a;NORMAL b;c])]
           else
               [APPL (intern_oriented_rule,[b;NORMAL a;c])])
       else
          (if is_constant b || is_default b then
               [APPL (intern_oriented_rule,[a;NORMAL b;c])]
           else if (!equality_test) a b then
               [APPL (intern_oriented_rule,[a;NORMAL b;c])]
           else if (!equality_test) b a then
               [APPL (intern_oriented_rule,[b;NORMAL a;c])]
           else
               [APPL (intern_unoriented_rule,[a;NORMAL b;c])])),
       ([APPL (s,[APPL (intern_equal,[a;b]);APPL (intern_true,[]);c])]))
  | (APPL (s,[APPL(11,[a;b]);
                           (APPL(4,[]));
                           c])) ->
      ([APPL (intern_unoriented_rule,[a;NORMAL b;c])],
       [APPL (s,[APPL (intern_equal,[a;b]);APPL (intern_true,[]);c])])
  (*| derive_rules (APPL (s,[APPL("==",[a,b]),
                           NORMAL (APPL("True",[])),
                           c])) =
    if (getMarkedVars a=[]) && (getMarkedVars b=[]) then
        ([APPL ("=",[a;NORMAL b;c])],
         [APPL (s,[APPL ("==",[a;b]);APPL ("True",[]);c])])
    else
        ([],[])*)
  | (APPL (s,[(QUANT (14,t,e,p));NORMAL (APPL(4,[]));c])) ->
    let e1 = Context.unmarkVarsList e (strip t) in
    let p1 = Context.unmarkVarsList p (strip t)
    in
        if p1=(APPL (intern_true,[])) then
            ([(APPL (s,[e1;NORMAL (APPL (4,[]));c]))],[])
        else
            ([(APPL (s,[(APPL (intern_or,[e1;APPL (intern_not,[p1])]));NORMAL (APPL (4,[]));c]))],[])
  | _ -> ([],[])
  ;;

let ra_derive e = match e with
  | (APPL (s,[APPL(9,l);NORMAL (APPL(5,[]));c])) ->
    (APPL (s,[APPL(intern_and,l);NORMAL (APPL(intern_false,[]));c]))::
    (mk_and_conds c s [] l)
  | (APPL (s,[APPL(10,l);NORMAL (APPL(4,[]));c])) ->
    (APPL (s,[APPL(intern_or,l);NORMAL (APPL(intern_true,[]));c]))::
    (mk_or_conds c s [] l)
  | x ->
    let rec d_rules x =
            let (a,b) = derive_rules x
            in
                if a=[] then x::b else (List.fold_left List.append [] (List.map d_rules a))@b in
    let res = d_rules x
    in
        if res=[] then [x] else res
    ;;

let user_derives env user_disc e =
    let user_rules = Disc.find (Env.isAorC env) user_disc e
    in
        List.fold_left List.append [] (List.map (fun (APPL (f,[pat;res;_])) ->
                  (List.map
                   (fun (u,el) -> Subst.subst u res)
                   (Match.thematch env pat e))) user_rules) ;;

let rec i_derive env user_disc e =
    let _ = Trace.trace "derive" (fun (xx) -> "d " ^ (prExp e)) in
    let (rules,residue) = derive_rules e
    in
        if rules=[] then
            let
                ur = user_derives env user_disc e
            in
                if ur=[] then
                    e::residue
                else
                    e::((List.fold_left List.append [] (List.map (i_derive env user_disc) ur))@residue)
        else
            (List.fold_left List.append [] (List.map (i_derive env user_disc) rules))@residue
    ;;

let generalize e = match e with
  | (APPL (4,[])) -> VAR (intern "condition")
  | (APPL (9,l)) -> (APPL (9,(VAR (intern "condition"))::l))
  | x -> (APPL (9,[VAR (intern "condition");x]))
  ;;

let make_user_rules env =
    (List.fold_left List.append [] (List.map
        (fun ([Env.E (APPL (f1,[l1;r1;c1]));
             Env.E (APPL (f2,[l2;r2;c2]))]) ->
            [(APPL (intern_unoriented_rule,[l1;r1;c1]),APPL (f2,[l2;r2;c2]));
             (APPL (intern_oriented_rule,[l1;r1;c1]),APPL (f2,[l2;r2;c2]));
             (APPL (intern_unoriented_rule,[l1;r1;generalize c1]),APPL (f2,[l2;r2;generalize c2]));
             (APPL (intern_oriented_rule,[l1;r1;generalize c1]),APPL (f2,[l2;r2;generalize c2]))])
(Env.allAttrib env intern_derive []))) ;;

let rec map_def e = match e with
  | (APPL (72,[x])) -> (APPL (72,[x]))
  | (APPL (72,l)) -> (APPL (71,l))
  | (APPL (f,l)) -> (APPL (f,List.map map_def l))
  | (QUANT (q,v,e,p)) -> (QUANT (q,v,map_def e,map_def p))
  | x -> x
  ;;

let rec derive env e = match e with
  | (REF x) ->
  (try (List.map (fun (x) -> (REF x)) (Cache.get_derived_rules x)) with Cache.NoEntry ->
    let r = derive env (ExpIntern.decode_exp (REF x)) in
        (*val _ = print ("cache deriving for " ^ (makestring x) ^ "\n")*)
    let r1 = List.map (ExpIntern.intern_exp (Env.isACorC env)) r in
    let r2 = List.map (fun (REF x) -> x) r1 in
        Cache.add_derived_rules x r2 ;
        r1)
  | (APPL (f,[l;r;c])) ->
    let (APPL (f,[l;r;c])) = map_def (APPL (f,[l;r;c])) in
    let e = (APPL (f,[l;r;c])) in
        (*val _ = print ("Deriving for " ^ (prExp e) ^ "\n")*)
    let user_rules = make_user_rules env in
    let user_disc = List.fold_right (fun (oo,n) -> (fun d ->
                                Disc.add (Env.isAorC env) d (APPL (intern_oriented_rule,[oo;n;(APPL (intern_true,[]))]))
                              )) user_rules Disc.newDisc in
    let _ = Trace.trace "derive" (fun (xx) -> "Deriving " ^ (prExp e)) in
    let _ = Trace.indent () in
    let res = List.filter
                   (fun (APPL (f,[l;r;c])) ->
                        not((remove_normals l)=(remove_normals r)))
                   (List.map
                   (fun (APPL (f,[l;r;c])) -> (APPL (f,[NORMAL l;r;c])))
                   (i_derive env user_disc (APPL (f,[l;NORMAL r;NORMAL c])))) in
    let _ = Trace.trace "derive" (fun (xx) -> "Result:") in
    let _ = Trace.indent () in
    let _ = Trace.trace_list "derive" (fun (x) -> List.map prExp res) in
    let _ = (Trace.undent () ; Trace.undent ()) in
    let _ = Trace.trace "derive" (fun (xx) -> "end derive for: " ^ (prExp e)) in
        res ;;

let rec rule_add_derive env (APPL (f,[l;r;c])) =
    let (APPL (f,[l;r;c])) = map_def (APPL (f,[l;r;c])) in
    let e = (APPL (f,[l;r;c])) in
    let _ = Trace.trace "derive" (fun (xx) -> "RA Deriving " ^ (prExp e)) in
    let _ = Trace.indent () in
    let user_rules = make_user_rules env in
    let user_disc = List.fold_right (fun (oo,n) -> (fun d ->
Disc.add (Env.isAorC env) d (APPL (intern_oriented_rule,[oo;n;(APPL (intern_true,[]))]))
                                 )) user_rules Disc.newDisc in
    let res = (List.filter
                  (fun (APPL (f,[l;r;c])) ->
                      not((remove_normals l)=(remove_normals r)))
                  (List.map
                  (fun (APPL (f,[l;r;c])) -> (APPL (f,[NORMAL l;r;c])))
                  (List.fold_left List.append [] (List.map
                  (fun (x) -> (i_derive env user_disc x))
                  (ra_derive (APPL (f,[l;NORMAL r; NORMAL c]))))))) in
    let _ = Trace.trace "derive" (fun (xx) -> "Result:") in
    let _ = Trace.indent () in
    let _ = Trace.trace_list "rderive" (fun (x) -> List.map prExp res) in
    let _ = (Trace.undent () ; Trace.undent ()) in
    let _ = Trace.trace "derive" (fun (xx) -> "end derive for: " ^ (prExp e))
    in
        res
    ;;



