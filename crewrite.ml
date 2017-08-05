(******************************************************************************
 *
 * REWRITELIB
 *
 * crewrite.ml
 *
 * This file contains the code for contextual rewriting.
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

(* require "crewri-s.sml" ;  *)
(* require "list.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "env.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "env.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "cache.sml" ;  *)
(* require "match.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "context.sml" ;  *)
(* require "derive.sml" ;  *)
(* require "trace.sml" ;  *)
(* require "basis.__list" ;  *)
(* open listimpl ; *)
open Exp ;;
open INTERNimpl ;;
(* open ENVimpl ; *)
(* open EXP_INTERNimpl ; *)
(* open CACHEimpl ; *)
(* open MATCHimpl ; *)
(* open SUBSTimpl ; *)
(* open CONTEXTimpl ; *)
(* open DERIVEimpl ; *)
(* open TRACEimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

let rec compute_constants e = match e with
  | (APPL (f,l)) ->
    List.fold_right List.append (List.map compute_constants l) []
  | (MARKED_VAR x) -> [(MARKED_VAR x)]
  | (QUANT (q,v,e,p)) ->
    (compute_constants e)@(compute_constants p)
  | (LET (v,t,e,p)) ->
    (compute_constants v)@(compute_constants e)@(compute_constants p)
  | (CASE (e,t,c)) ->
    (compute_constants e)@
    (List.fold_left List.append []
        (List.map (fun (p,e) -> (compute_constants p)@(compute_constants e)) c))
  | (INDEX (e,i1,i2)) -> compute_constants e
  | (HIGHLIGHT e) -> compute_constants e
  | (NORMAL e) -> compute_constants e
  | (NUM i) -> [(NUM i)]
  | (STRING s) -> [(STRING s)]
  | (CHAR c) -> [(CHAR c)]
  | (REF i) -> compute_constants (decode_exp (REF i))
  | _ -> [] ;;

let compute_env_constants env =
    if getContextList env=[] then
        (try List.map (fun (x) -> InternExp.decode_exp (REF x)) (get_relevant_constants [])
         with NoEntry ->
         let res = List.fold_right List.append []
             (List.map compute_constraints
                 (List.map ExpIntern.decode_exp (getAllRules env)))
         in
            add_relevant_constants []
                (res <> (fn (x) => intern_exp (isACorC env) x)
                     <> (fn (REF x) => x)) ;
            res)
    else
        (try (map (fn (x) => decode_exp (REF x)) (get_relevant_constants (getContextList env)))
          with NoEntry ->
          let res = List.fold_right List.append [] ((List.map
                            (fun (x) -> decode_exp (REF x))))
                            (getAllRules env)@(List.map compute_constants
                                (List.map
                                    (fun (APPL (_,[l,r,c])) -> r)
                                    (getContextList env)))
          in
              add_relevant_constants (getContextList env)
                (List.map
                     (fun (REF x) -> x)
                     (List.map (fun (x) -> intern_exp (isACorC env) x) res)) ;
              res
          )

let old_relevant_rule env exp rule =
   (try let REF n = intern_exp (isACorC env) rule
    in
        has_relevant_constants (getContextList env) n
    with NoEntry ->
    let constants = compute_env_constants env in
    let (APPL (_,[l,_,_])) = (decode_exp rule) in
    let rule_constants = (remove_dups (compute_constants l)) in
    let exp = (decode_exp exp) in
    let exp_constants = (List.remove_dups (compute_constants exp))@
                        (List.map (fun (x) -> (MARKED_VAR x)) (getFreeVars exp)) in
    let res = (List.difference (List.difference rule_constants exp_constants) constants = []) in
    let REF n = intern_exp (isACorC env) rule in
        add_has_relevant_constants (getContextList env) n res ;
        res
    )

let rec has_default e = match e with
  | (APPL (f,l)) ->
    f=71 orelse f=72 orelse has_default_list l
  | (QUANT (q,t,e,p)) ->
    has_default e orelse has_default p
  | (NORMAL x) -> has_default x
  | (REF x) -> has_default (decode_one_exp (REF x))
  | _ -> false
and has_default_list l = match l with
  | [] -> false
  | (f::r) -> has_default f orelse has_default_list r

let rec get_default_clause e = match e with
  | (APPL (f,l)) ->
    if f=71 orelse f=72 then [hd l] else get_default_clause_list l
  | (QUANT (q,t,e,p)) ->
    (get_default_clause e)@(get_default_clause p)
  | (NORMAL x) -> get_default_clause x
  | (REF x) -> get_default_clause (decode_exp (REF x))
  | _ -> nil
and get_default_clause_list l = match l with
  | [] -> []
  | (f::r) -> (get_default_clause f)@(get_default_clause_list r)

fun relevant_rule env (REF e) (REF r) =
   (has_relevant_constants [r] e
    handle NoEntry =>
    let val x = relevant_rule env (decode_exp (REF e)) (decode_exp (REF r))
    in
        add_has_relevant_constants [r] e x ; x
    end)
  | relevant_rule env exp (APPL (_,[l,r,c])) =
    if has_default exp then true else let val l = markVars l
    in
        case l
          of (APPL (f,[a,b])) => if ((getAttrib env intern_po [S(f)])@
                                     (getAttrib env intern_to [S(f)])@
                                     (getAttrib env intern_epo [S(f)])@
                                     (getAttrib env intern_eto [S(f)]))=nil
                                 then
                                     equal_smaller env l exp
                                 else true
           | _                => equal_smaller env l exp
    end
  | relevant_rule env exp (REF x) = relevant_rule env exp (decode_exp (REF x))
  | relevant_rule _ _ _ = false

let relevant_rule2 env (REF e) (REF r) =
   (has_relevant_constants [r] e
    handle NoEntry =>
    let val x = relevant_rule2 env (decode_exp (REF e)) (decode_exp (REF r))
    in
        add_has_relevant_constants [r] e x ; x
    end)
  | relevant_rule2 env exp (APPL (_,[l,r,c])) =
    if has_default exp then true else let val l = markVars l
    in
        much_smaller env l exp orelse
        (case l
           of (APPL (11,[a,b])) => DERIVEimpl.equality_terms a b orelse
                                   DERIVEimpl.equality_terms b a
            | _                 => false)
    end
  | relevant_rule2 env exp (REF x) = relevant_rule2 env exp (decode_exp (REF x))
  | relevant_rule2 _ _ _ = false

fun relevant_rule_list env nil r = false
  | relevant_rule_list env (f::s) r =
    (relevant_rule env f (REF r)) orelse (relevant_rule_list env s r)

fun relevant_rule_list2 env nil r = false
  | relevant_rule_list2 env (f::s) r =
    (relevant_rule2 env f (REF r)) orelse (relevant_rule_list2 env s r)

fun filter_rule_list env exp rules =
    let val dfcl = foldl append nil (map (fn (x) => get_default_clause (REF x)) rules)
        val _ = trace_list "rewriteRule" (fn (x) => (map (fn (x) => "dr " ^ (prExp x)) dfcl))
    in
        rules |> (fn (x) => relevant_rule_list env (exp::dfcl) x)
    end

fun junction_filter_rule_list env exp rules =
    let val dfcl = foldl append nil (map (fn (x) => get_default_clause (REF x)) rules)
        val _ = trace_list "rewriteRule" (fn (x) => (map (fn (x) => "dr " ^ (prExp x)) dfcl))
    in
        rules |> (fn (x) => relevant_rule_list2 env (exp::dfcl) x)
    end

fun orient_rule env (APPL (f,[l,r,c])) =
    if equal_smaller env r l andalso equal_smaller env c l then
        [APPL (intern_oriented_rule,[l,r,c]),APPL (intern_unoriented_rule,[r,l,c])]
    else if equal_smaller env l r andalso equal_smaller env c r then
        [APPL (intern_oriented_rule,[r,l,c]),APPL (intern_unoriented_rule,[l,r,c])]
    else
        [APPL (intern_unoriented_rule,[l,r,c]),APPL (intern_unoriented_rule,[r,l,c])]
  | orient_rule env (REF x) = orient_rule env (decode_exp (REF x))
  | orient_rule env x = [x]
  ;

fun add_rule rewrite env exp rule =
    (trace "rewrite" (fn (x) => "Adding " ^ (prExp exp) ^ "\n") ;
     addContextRules env
     ((derive env (intern_exp (isACorC env) rule)) |>
      (*(relevant_rule env exp) |>*)
      (fn (r) => (case (decode_exp r)
                    of (APPL (f,[APPL (4,nil),_,c])) => false
                     | (APPL (f,[APPL (5,nil),_,c])) => false
                     | (APPL (1,[l,r,c])) => not(l=r)
                     | _ => false)))) ;

fun create_rules rewrite env (APPL (9,l)) n =
    add_rule rewrite env (List.nth (l,n)) (APPL (intern_oriented_rule,[(APPL (intern_and,map markVars (delete_nth (l,n)))),
                                      (APPL (intern_true,nil)),
                                      (APPL (intern_true,nil))]))
  | create_rules rewrite env (APPL (10,l)) n =
    add_rule rewrite env (List.nth (l,n)) (APPL (intern_oriented_rule,[(APPL (intern_or,map markVars (delete_nth (l,n)))),
                              (APPL (intern_false,nil)),
                              (APPL (intern_true,nil))]))
  | create_rules rewrite env (APPL (18,[c,e1,e2])) 1 =
    add_rule rewrite env e1 (APPL (intern_oriented_rule,[markVars c,(APPL (intern_true,nil)),(APPL (intern_true,nil))]))
  | create_rules rewrite env (APPL (18,[c,e1,e2])) 2 =
    add_rule rewrite env e2 (APPL (intern_oriented_rule,[markVars c,(APPL (intern_false,nil)),(APPL (intern_true,nil))]))
  | create_rules rewrite env (APPL (1,[l,r,c])) 0 =
    add_rule rewrite env l (APPL (intern_oriented_rule,[markVars c,(APPL (intern_true,nil)),(APPL (intern_true,nil))]))
  | create_rules rewrite env (APPL (1,[l,r,c])) 1 =
    add_rule rewrite env r (APPL (intern_oriented_rule,[markVars c,(APPL (intern_true,nil)),(APPL (intern_true,nil))]))
  | create_rules rewrite env (APPL (2,[l,r,c])) 0 =
    add_rule rewrite env l (APPL (intern_oriented_rule,[markVars c,(APPL (intern_true,nil)),(APPL (intern_true,nil))]))
  | create_rules rewrite env (APPL (2,[l,r,c])) 1 =
    add_rule rewrite env r (APPL (intern_oriented_rule,[markVars c,(APPL (intern_true,nil)),(APPL (intern_true,nil))]))
  | create_rules _ env _ _ = env
  ;





