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
open Intern ;;
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
  | (REF i) -> compute_constants (ExpIntern.decode_exp (REF i))
  | _ -> [] ;;

let compute_env_constants env =
    if Env.getContextList env=[] then
        (try List.map (fun (x) -> ExpIntern.decode_exp (REF x)) (Cache.get_relevant_constants [])
         with Cache.NoEntry ->
         let res = List.fold_right List.append
             (List.map compute_constants
                 (List.map ExpIntern.decode_exp (Env.getAllRules env))) []
         in
            Cache.add_relevant_constants []
                (List.map
                    (fun (REF x) -> x)
                    (List.map
                        (fun (x) -> ExpIntern.intern_exp (Env.isACorC env) x)
                        res)) ;
            res)
    else
        (try (List.map (fun (x) -> ExpIntern.decode_exp (REF x)) (Cache.get_relevant_constants (Env.getContextList env)))
          with Cache.NoEntry ->
          let res = List.fold_right List.append
                            (List.map compute_constants
                                (List.map (fun (APPL (_,[l;r;c])) -> r)
                                    ((Env.getAllRules env)@
                                     (List.map
                                         (fun (x) -> ExpIntern.decode_exp (REF x))
                                         (Env.getContextList env))))) []
          in
              Cache.add_relevant_constants (Env.getContextList env)
                (List.map
                     (fun (REF x) -> x)
                     (List.map (fun (x) -> ExpIntern.intern_exp (Env.isACorC env) x) res)) ;
              res
          )

let old_relevant_rule env exp rule =
   (try let REF n = ExpIntern.intern_exp (Env.isACorC env) rule
    in
        Cache.has_relevant_constants (Env.getContextList env) n
    with Cache.NoEntry ->
    let constants = compute_env_constants env in
    let (APPL (_,[l;_;_])) = (ExpIntern.decode_exp rule) in
    let rule_constants = (Mylist.remove_dups (compute_constants l)) in
    let exp = (ExpIntern.decode_exp exp) in
    let exp_constants = (Mylist.remove_dups (compute_constants exp))@
                        (List.map (fun (x) -> (MARKED_VAR x)) (Rcontext.getFreeVars exp)) in
    let res = (Mylist.difference (Mylist.difference rule_constants exp_constants) constants = []) in
    let REF n = ExpIntern.intern_exp (Env.isACorC env) rule in
        Cache.add_has_relevant_constants (Env.getContextList env) n res ;
        res
    )

let rec has_default e = match e with
  | (APPL (f,l)) ->
    f=71 || f=72 || has_default_list l
  | (QUANT (q,t,e,p)) ->
    has_default e || has_default p
  | (NORMAL x) -> has_default x
  | (REF x) -> has_default (ExpIntern.decode_one_exp (REF x))
  | _ -> false
and has_default_list l = match l with
  | [] -> false
  | (f::r) -> has_default f || has_default_list r

let rec get_default_clause e = match e with
  | (APPL (f,l)) ->
    if f=71 || f=72 then [List.hd l] else get_default_clause_list l
  | (QUANT (q,t,e,p)) ->
    (get_default_clause e)@(get_default_clause p)
  | (NORMAL x) -> get_default_clause x
  | (REF x) -> get_default_clause (ExpIntern.decode_exp (REF x))
  | _ -> []
and get_default_clause_list l = match l with
  | [] -> []
  | (f::r) -> (get_default_clause f)@(get_default_clause_list r)

let rec relevant_rule env e r = match (e,r) with
  | ((REF e),(REF r)) ->
   (try Cache.has_relevant_constants [r] e
    with Cache.NoEntry ->
    let x = relevant_rule env (ExpIntern.decode_exp (REF e)) (ExpIntern.decode_exp (REF r))
    in
        Cache.add_has_relevant_constants [r] e x ; x
    )
  | (exp,(APPL (_,[l;r;c]))) ->
    if has_default exp then true else let l = Rcontext.markVars l
    in
        (match l with
           | (APPL (f,[a;b])) -> if ((Env.getAttrib env intern_po [S(f)])@
                                     (Env.getAttrib env intern_to [S(f)])@
                                     (Env.getAttrib env intern_epo [S(f)])@
                                     (Env.getAttrib env intern_eto [S(f)]))=[]
                                 then
                                     Match.equal_smaller env l exp
                                 else true
           | _                -> Match.equal_smaller env l exp)
  | (exp,(REF x)) -> relevant_rule env exp (ExpIntern.decode_exp (REF x))
  | (_,_) -> false ;;

let rec relevant_rule2 env e r = match (e,r) with
  | ((REF e),(REF r)) ->
   (try Cache.has_relevant_constants [r] e
    with Cache.NoEntry ->
    let x = relevant_rule2 env (ExpIntern.decode_exp (REF e)) (ExpIntern.decode_exp (REF r))
    in
        Cache.add_has_relevant_constants [r] e x ; x
    )
  | (exp,(APPL (_,[l;r;c]))) ->
    if has_default exp then true else let l = Rcontext.markVars l
    in
        Match.much_smaller env l exp ||
        (match l with
            | (APPL (11,[a;b])) -> Derive.equality_terms a b ||
                                   Derive.equality_terms b a
            | _                 -> false)
  | (exp,(REF x)) -> relevant_rule2 env exp (ExpIntern.decode_exp (REF x))
  | (_,_) -> false ;;

let rec relevant_rule_list env l r = match l with
  | [] -> false
  | (f::s) ->
    (relevant_rule env f (REF r)) || (relevant_rule_list env s r) ;;

let rec relevant_rule_list2 env l r = match l with
  | [] -> false
  | (f::s) ->
    (relevant_rule2 env f (REF r)) || (relevant_rule_list2 env s r) ;;

let rec filter_rule_list env exp rules =
    let dfcl = List.fold_left List.append [] (List.map (fun (x) -> get_default_clause (REF x)) rules) in
    let _ = Rtrace.trace_list "rewriteRule" (fun (x) -> (List.map (fun (x) -> "dr " ^ (prExp x)) dfcl))
    in
        List.filter (fun (x) -> relevant_rule_list env (exp::dfcl) x) rules
    ;;

let junction_filter_rule_list env exp rules =
    let dfcl = List.fold_left List.append [] (List.map (fun (x) -> get_default_clause (REF x)) rules) in
    let _ = Rtrace.trace_list "rewriteRule" (fun (x) -> (List.map (fun (x) -> "dr " ^ (prExp x)) dfcl)) in
        List.filter (fun (x) -> relevant_rule_list2 env (exp::dfcl) x) rules
    ;;

let rec orient_rule env e = match e with
  | (APPL (f,[l;r;c])) ->
    if Match.equal_smaller env r l && Match.equal_smaller env c l then
        [APPL (intern_oriented_rule,[l;r;c]);APPL (intern_unoriented_rule,[r;l;c])]
    else if Match.equal_smaller env l r && Match.equal_smaller env c r then
        [APPL (intern_oriented_rule,[r;l;c]);APPL (intern_unoriented_rule,[l;r;c])]
    else
        [APPL (intern_unoriented_rule,[l;r;c]);APPL (intern_unoriented_rule,[r;l;c])]
  | (REF x) -> orient_rule env (ExpIntern.decode_exp (REF x))
  | x -> [x]
  ;;

let add_rule rewrite env exp rule =
    (Rtrace.trace "rewrite" (fun (x) -> "Adding " ^ (prExp exp) ^ "\n") ;
     Env.addContextRules env
     (List.filter
      (*(relevant_rule env exp) |>*)
      (fun (r) -> (match (ExpIntern.decode_exp r) with
                     | (APPL (f,[APPL (4,[]);_;c])) -> false
                     | (APPL (f,[APPL (5,[]);_;c])) -> false
                     | (APPL (1,[l;r;c])) -> not(l=r)
                     | _ -> false))
      (Derive.derive env (ExpIntern.intern_exp (Env.isACorC env) rule)))) ;;

let create_rules rewrite env e n = match (e,n) with
  | ((APPL (9,l)),n) ->
    add_rule rewrite env (List.nth l n) (APPL (intern_oriented_rule,[(APPL (intern_and,List.map Rcontext.markVars (Mylist.delete_nth l n)));
                                      (APPL (intern_true,[]));
                                      (APPL (intern_true,[]))]))
  | ((APPL (10,l)),n) ->
    add_rule rewrite env (List.nth l n) (APPL (intern_oriented_rule,[(APPL (intern_or,List.map Rcontext.markVars (Mylist.delete_nth l n)));
                              (APPL (intern_false,[]));
                              (APPL (intern_true,[]))]))
  | ((APPL (18,[c;e1;e2])),1) ->
    add_rule rewrite env e1 (APPL (intern_oriented_rule,[Rcontext.markVars c;(APPL (intern_true,[]));(APPL (intern_true,[]))]))
  | ((APPL (18,[c;e1;e2])),2) ->
    add_rule rewrite env e2 (APPL (intern_oriented_rule,[Rcontext.markVars c;(APPL (intern_false,[]));(APPL (intern_true,[]))]))
  | ((APPL (1,[l;r;c])),0) ->
    add_rule rewrite env l (APPL (intern_oriented_rule,[Rcontext.markVars c;(APPL (intern_true,[]));(APPL (intern_true,[]))]))
  | ((APPL (1,[l;r;c])),1) ->
    add_rule rewrite env l (APPL (intern_oriented_rule,[Rcontext.markVars c;(APPL (intern_true,[]));(APPL (intern_true,[]))]))
  | ((APPL (2,[l;r;c])),0) ->
    add_rule rewrite env l (APPL (intern_oriented_rule,[Rcontext.markVars c;(APPL (intern_true,[]));(APPL (intern_true,[]))]))
  | ((APPL (2,[l;r;c])),1) ->
    add_rule rewrite env r (APPL (intern_oriented_rule,[Rcontext.markVars c;(APPL (intern_true,[]));(APPL (intern_true,[]))]))
  | (_,_) -> env
  ;;





