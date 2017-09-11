(******************************************************************************
 *                          
 * REWRITELIB           
 *                          
 * rule_app.ml 
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
(* require "env.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "match.sml" ;  *)
(* require "context.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "trace.sml" ;  *)
(* require "derive.sml" ;  *)
(* require "cache.sml" ;  *)
(* require "disc.sml" ;  *)
(* require "rule_a-s.sml" ;  *)
(* require "crewrite.sml" ;  *)
(* require "basis.__integer" ;  *)

(* open listimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

open Env ;;
open Exp ;;
open Intern ;;
(* open EXP_INTERNimpl ; *)
(* open MATCHimpl ; *)
(* open CONTEXTimpl ; *)
(* open SUBSTimpl ; *)
(* open TRACEimpl ; *)
(* open DERIVEimpl ; *)
(* open CACHEimpl ; *)
(* open DISCimpl ; *)

let rec equal_list env l1 l2 = match (l1,l2) with
  | ([],[]) -> true
  | ((a::b),(c::d)) ->
    Match.equal env a c && equal_list env b d
  ;;

let rec separate_matches l = match l with
  | [] -> ([],[])
  | ((a,b)::r) ->
    let (r1,r2)=separate_matches r
    in
        if b=[] then
            ((a,b)::r1,r2)
        else
            (r1,(a,b)::r2)
    ;;

let rec cutoff l x = match (l,x) with
  | ([],_) -> []
  | (x,0) -> []
  | ((a::b),n) -> a::(cutoff b (n-1))
  ;;

let orient_rule env e = match e with
  | (APPL (f,[l;r;c])) ->
    (let mvc = Mylist.difference (Rcontext.getFreeVars c) (Rcontext.getFreeVars l) in
    let c1 = Rcontext.markVarsList c mvc in
    let mvc = Mylist.difference (Rcontext.getFreeVars c) (Rcontext.getFreeVars r) in
    let c2 = Rcontext.markVarsList c mvc in
    let mvl = Mylist.difference (Rcontext.getFreeVars l) (Rcontext.getFreeVars r) in
    let l1 = Rcontext.markVarsList l mvl in
    let mvc = Mylist.difference (Rcontext.getFreeVars r) (Rcontext.getFreeVars l) in
    let r1 = Rcontext.markVarsList r mvc in
        if (Match.equal_smaller env r1 l) && not(Match.equal_smaller env l r1) && (Match.equal_smaller env c1 l) &&
           not(Match.equal_smaller env l c1) then
            APPL (intern_oriented_rule,[l;r;c])
        else if (Match.equal_smaller env c2 r) && not(Match.equal_smaller env r c2) && (Match.equal_smaller env l1 r)
                && not(Match.equal_smaller env r l1)  then
            APPL (intern_oriented_rule,[r;l;c])
        else
            APPL (intern_unoriented_rule,[l;r;c]))
  | x -> x
  ;;

let add_rule rewrite env rule =
    List.fold_right (fun p -> (fun e -> addProperty e p))
          (List.map (orient_rule env)
              (List.fold_left List.append [] (List.map rewrite
                  (Derive.derive env rule)))
              ) env ;;

let rec add_and_derive rewrite env l = match l with
  | [x] ->
    add_rule (rewrite env) env (APPL (intern_oriented_rule,[x;(APPL (intern_true,[]));
                                              (APPL (intern_true,[]))]))
  | l ->
    add_rule (rewrite env) env (APPL (intern_oriented_rule,[(APPL (intern_and,l));
                                            (APPL (intern_true,[]));
                                            (APPL (intern_true,[]))])) ;;

let rec add_or_derive rewrite env l = match l with
  | [x] ->
    add_rule (rewrite env) env (APPL (intern_oriented_rule,[x;(APPL (intern_false,[]));
                                              (APPL (intern_true,[]))]))
  | l ->
    add_rule (rewrite env) env (APPL (intern_oriented_rule,[(APPL (intern_or,l));
                                            (APPL (intern_false,[]));
                                            (APPL (intern_true,[]))])) ;;

let rec build_env rewrite env e residue = match e with
  | (APPL (s,l)) ->
    let env1 =
            if residue=[] then
                env
            else if s=intern_and then
                add_and_derive rewrite env residue
            else if s=intern_or then
                add_or_derive rewrite env residue
            else
                env
    in
        env1
  | _ -> env
  ;;

let test_condition rewrite env f cond sub residue =
    List.mem (APPL (intern_true,[]))
        (List.map ExpIntern.decode_exp
            (rewrite (build_env rewrite env f residue)
                     (Rcontext.unmarkVars (Rsubst.subst sub cond))))
  ;;

let rec find_first rewrite env l e c = match l with
  | [] -> []
  | ((x,ll)::r) ->
    if test_condition rewrite env e c x ll then
        [(x,ll)]
    else
        find_first rewrite env r e c
  ;;

let extended_subst sub r vars =
    if vars=[] then
        Rsubst.subst sub r
    else
        (let residue = Mylist.difference (Rcontext.getFreeVars r) (Rsubst.dom sub) in
        let _ = Rtrace.trace "rewriteRule" (fun (xx) -> ("enter subst: " ^ (prExp r))) in
        let sub2 = List.fold_right
                            (fun v -> (fun s ->
                                Rsubst.addPair s v
                                    (MARKED_VAR (Rcontext.name_away_from_list
                                        (vars @ (Rsubst.dom s))
                                        v
                                    ))
                            )) residue sub in
        let res = Rsubst.subst sub2 (Rcontext.unmarkVars r) in
        let _ = Rtrace.trace "rewriteRule" (fun (xx) -> ("subst: " ^ (prExp res))) in
        let _ = (Rtrace.indent () ; Rtrace.undent ()) in
            res)
        ;;

let rec delete_subs a l x vars env = match l with
  | [] -> []
  | (b::c) ->
    let (a1,a2) = a in
    let (b1,b2) = b in
        if Match.equal env (extended_subst a1 x vars)
                     (extended_subst b1 x vars) &&
           equal_list env a2 b2 then
            delete_subs a c x vars env
        else
            b::(delete_subs a c x vars env)
   ;;

let rec remove_sub_dups l x vars env = match l with
  | [] -> []
  | (a::b) ->
    a::(remove_sub_dups (delete_subs a b x vars env) x vars env)
  ;;

let possibleRewritesUsingRule rewrite env e vars ee = match ee with
  | (APPL (s,[l;r;c])) ->
    let matches = Match.thematch env l e
    in
        if matches=[] then
            []
        else
            (let _ = Rtrace.trace "rewriteRule"
                (fun (x) -> "Applying " ^ (prExp (APPL (s,[l;r;c]))) ^
                           " to " ^ (prExp e)) in
            let _ = Rtrace.indent () in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match count: " ^ (string_of_int (List.length matches))) in
            let (ma,mb) = separate_matches matches in
            let matches2 = cutoff (ma@mb) 10 in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match2 count: " ^ (string_of_int (List.length matches2))) in
            let matches3 = remove_sub_dups matches2 (APPL (s,[l;r;c])) vars env in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match3 count: " ^ (string_of_int (List.length matches3))) in
            let _ = (Rtrace.indent () ; Rtrace.undent ()) in
            let res =
                    List.map (fun (x,l) ->
                            let res = extended_subst x r vars in
                            let _ = Rtrace.trace "rewriteRule"
                                        (fun (xx) -> ("r: " ^  (prExp res))) in
                            let _ = Rtrace.indent () in
                            let _ = Rtrace.trace_list "rewriteRule"
                                        (fun (xx) -> List.map prExp l) in
                            let _ = Rtrace.undent () in
                            let fres =
                                    (if l = [] then
                                        res
                                    else
                                        let (APPL (s,_)) = e in
                                        (APPL (s,res::l))) in
                                (fres,build_env rewrite env e l,
                                 (extended_subst x c vars),l)
                        )
                        matches3 in
            let _ = Rtrace.trace "rewriteRule"
                  (fun (xx) -> "Result count: " ^ (string_of_int (List.length res))) in
            let _ = Rtrace.undent () in
            let _ = Rtrace.trace "rewriteRule"
                (fun (x) -> "End apply " ^ (prExp (APPL (s,[l;r;c]))) ^
                           " to " ^ (prExp e)) in
            res)
  | _ -> [] ;;

let pr_theta theta =
    List.fold_left ( ^ ) "" (List.map (fun (x) -> "["^(Intern.decode x)^"->"^(prExp (Rsubst.apply theta x))^"]") (Rsubst.dom theta)) ;;

let identity_theta theta =
    let x = Rsubst.dom theta in
    let rec id l = match l with
          | [] -> true
          | (f::r) -> ((VAR f)=Rsubst.apply theta f ||
                         (MARKED_VAR f)=Rsubst.apply theta f) && id r
    in
        id x
    ;;

let rec unify_refine rewrite env c theta =
    let _ = Rtrace.trace "rewriteRule" (fun (x) -> ("u: " ^ (prExp c))) in
    let _ = Rtrace.trace "rewriteRule" (fun (x) -> ("t: " ^ (pr_theta theta))) in
    let _ = Rtrace.trace_list "rewriteRule" (fun (x) -> List.map (fun (x) -> ("cr: " ^ (prExp (ExpIntern.decode_exp (REF x))))) (Env.getContextList env)) in
    let rec terms = match c with
                       | (APPL (9,l)) -> l
                       | _            -> [c] in
    let rec fu l = match l with
          | [] -> Rsubst.empty
          | (f::r) ->
            let _ = Rtrace.trace "rewriteRule" (fun (x) -> ("test: " ^ (prExp f))) in
            let (REF num) = ExpIntern.intern_exp (Env.isACorC env) f in
            let rules0 = try Cache.get_urules num with Cache.NoEntry ->
                    let fr = Disc.ufind (Env.isAorC env) (Env.getRules env) (ExpIntern.decode_exp f) in
                    let fr2 = List.map (fun (x) -> let (REF e)=ExpIntern.intern_exp (isACorC env) x in e) fr in
                        Cache.add_urules num fr2 ;
                        fr2 in
            let rules1 = try Cache.get_context_rules num (getContextList env) with Cache.NoEntry ->
                    (let rl = Disc.findSmall (Env.getContextDisc env) (ExpIntern.decode_exp f) in
                    let rl1 = List.map (fun (x) -> let (REF e)=ExpIntern.intern_exp (isACorC env) x in e) rl in
                        (Cache.add_context_rules num (Env.getContextList env) rl1 ;
                        rl1)
                    ) in
            let rules = List.map (fun (x) -> ExpIntern.decode_exp (REF x)) (Mylist.remove_dups (rules1@rules0)) in
            let _ = Rtrace.trace_list "rewriteRule"
                        (fun (x) -> List.map prExp rules) in
            let rec fm l = match l with
                  | [] -> fu r
                  | ((APPL (fu,[l;r;cc]))::rest) ->
                    let mat = List.filter (fun (a,b,c,d) -> c=[] && d=[])
                              (Match.unify env l f) in
                    let _ = Rtrace.trace "rewriteRule" (fun (x) -> ("Unified " ^ (prExp l) ^ " " ^ (prExp f))) in
                    let rec fm2 l = match l with
                          | [] -> fm rest
                          | (f::r) ->
                            let (_,theta2,_,_) = f in
                            let itheta = identity_theta theta2 in
                            let _ = Rtrace.trace "rewriteRule" (fun (x) -> pr_theta theta2) in
                            let c = Rsubst.subst theta2 c in
                            let _ = Rtrace.trace "rewriteRule" (fun (x) -> ("c = " ^ (prExp c))) in
                            let rr = if itheta then c else List.hd ((rewrite env c)@[c]) in
                            let comp = Rsubst.compose theta2 theta in
                            let _ = Rtrace.trace "rewriteRule " (fun (x) -> ("c = " ^ (prExp c) ^ " rr = " ^ (prExp rr))) in
                            let res = if itheta || (ExpIntern.decode_exp rr)=(APPL (intern_true,[])) then
                                          comp
                                      else
                                          unify_refine rewrite env c comp
                            in
                                if itheta || res=Rsubst.empty then fm2 r else res
                    in
                        fm2 mat
            in
                fm rules
    in
        fu terms
    ;;

let unify_refine_cache rewrite env c =
    let (REF x) = ExpIntern.intern_exp (Env.isACorC env) c
    in
        try Cache.get_unify_refine x (Env.getContextList env) with Cache.NoEntry ->
        (let theta = unify_refine rewrite env c Rsubst.empty
         in
             Cache.add_unify_refine x (Env.getContextList env) theta ;
             theta
         )
    ;;

let rec map_def e = match e with
  | (APPL (72,l)) -> (APPL (71,l))
  | (APPL (f,l)) -> (APPL (f,List.map map_def l))
  | x -> x ;;

let rewriteUsingRuleUnify rewrite env e vars ee = match ee with
  | (APPL (s,[l;r;c])) ->
    let matches = Match.thematch env l e
    in
        if matches=[] then
            []
        else
            let _ = Rtrace.trace "rewriteRule"
                (fun (x) -> "Applying unify " ^ (prExp (APPL (s,[l;r;c]))) ^
                           " to " ^ (prExp e)) in
            let _ = Rtrace.indent () in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match count: " ^ (string_of_int (List.length matches))) in
            let (ma,mb) = separate_matches matches in
            let matches2 = cutoff (ma@mb) 10 in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match2 count: " ^ (string_of_int (List.length matches2))) in
            let matches3 = remove_sub_dups matches2 (APPL (s,[l;r;c])) vars env in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match3 count: " ^ (string_of_int (List.length matches3))) in
            let _ = (Rtrace.indent () ; Rtrace.undent ()) in
            let matches4 = List.map (fun (s,r) -> (Rsubst.addNormals s,r)) matches3 in
            let res =
                    List.map (fun (x,l,u) ->
                            let res = Rcontext.unmarkUnnormalVars (extended_subst x (Rsubst.subst u r) vars) in
                            let _ = Rtrace.trace "rewriteRule"
                                        (fun (xx) -> ("r: " ^  (prExp res))) in
                            let _ = Rtrace.indent () in
                            let _ = Rtrace.trace_list "rewriteRule"
                                        (fun (xx) -> List.map prExp l) in
                            let _ = Rtrace.undent () in
                                if l = [] then
                                    res
                                else
                                    let (APPL (s,_)) = e in
                                        (APPL (s,res::l))
                        )
                        (List.fold_left List.append [] (List.map
                         (fun (x,ll) ->
                          let c = Rsubst.subst x c in
                          let c1 = ExpIntern.intern_exp (Env.isACorC env) c in
                          let _ = Rtrace.trace "rewriteRule" (fun (x) -> "START UNIFY") in
                          (*val l = CREWRITEimpl.filter_rule_list env c1 (getContextList env)*)
                          let l = (Env.getContextList env) in
                          let env = Env.addContextRules (Env.clearContextRules env) (List.map (fun (x) -> REF x) l) in
                          let _ = Rtrace.trace_list "rewriteRule" (fun (x) -> List.map (fun (r) -> "pr: " ^ (prExp (ExpIntern.decode_exp (REF r)))) (Env.getContextList env)) in
                          let _ = Rtrace.trace_list "rewriteRule" (fun (x) -> List.map (fun (r) -> "r: " ^ (prExp (ExpIntern.decode_exp (REF r)))) l) in
                          let s = unify_refine_cache rewrite env (map_def c) in
                          let _ = Rtrace.trace "rewriteRule" (fun (x) ->"DONE UNIFY") in
                              if s=Rsubst.empty then [] else [(x,ll,s)]
                         ) matches4)
                        ) in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Result count: " ^ (string_of_int (List.length res))) in
            let _ = Rtrace.undent () in
            let _ = Rtrace.trace "rewriteRule"
                (fun (x) -> "End apply " ^ (prExp (APPL (s,[l;r;c]))) ^
                           " to " ^ (prExp e)) in
                res
    ;;

let rewriteUsingRule rewrite env e vars ee = match ee with
  | (APPL (s,[l;r;c])) ->
    let matches = Match.thematch env l e
    in
        if matches=[] then
            []
        else
            let _ = Rtrace.trace "rewriteRule"
                (fun (x) -> "Applying " ^ (prExp (APPL (s,[l;r;c]))) ^
                           " to " ^ (prExp e)) in
            let _ = Rtrace.indent () in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match count: " ^ (string_of_int (List.length matches))) in
            let (ma,mb) = separate_matches matches in
            let matches2 = cutoff (ma@mb) 10 in
            let _ = Rtrace.trace "rewriteRule"
                      (fun (xx) -> "Match2 count: " ^ (string_of_int (List.length matches2))) in
            let matches3 = remove_sub_dups matches2 (APPL (s,[l;r;c])) vars env in
            let _ = Rtrace.trace "rewriteRule"
                    (fun (xx) -> "Match3 count: " ^ (string_of_int (List.length matches3))) in
            let _ = (Rtrace.indent () ; Rtrace.undent ()) in
            let matches4 = List.map (fun (s,r) -> (Rsubst.addNormals s,r)) matches3 in
            let res =
                    List.map (fun (x,l) ->
                            let res = Rcontext.unmarkUnnormalVars (extended_subst x r vars) in
                            let _ = Rtrace.trace "rewriteRule"
                                    (fun (xx) -> ("r: " ^  (prExp res))) in
                            let _ = Rtrace.indent () in
                            let _ = Rtrace.trace_list "rewriteRule"
                                           (fun (xx) -> List.map prExp l) in
                            let _ = Rtrace.undent () in
                                if l = [] then
                                    res
                                else
                                    let (APPL (s,_)) = e in
                                        (APPL (s,res::l))
                        )
                        (if isSingularRule env (APPL (s,[l;r;c])) then
                            find_first rewrite env matches4 e c
                        else
                            (List.filter
                             (fun (x,ll) -> test_condition rewrite env e (map_def c) x ll) matches4
                            )) in
            let _ = Rtrace.trace "rewriteRule"
                  (fun (xx) -> "Result count: " ^ (string_of_int (List.length res))) in
            let _ = Rtrace.undent () in
            let _ = Rtrace.trace "rewriteRule"
                (fun (x) -> "End apply " ^ (prExp (APPL (s,[l;r;c]))) ^
                           " to " ^ (prExp e)) in
                res
  | _ -> [] ;;

let introduced_vars x =
    try (Cache.get_introduced_vars x) with Cache.NoEntry ->
    let e = ExpIntern.decode_exp (REF x) in
    let (APPL (_,[l;r;c])) = e in
    let iv = Mylist.remove_dups (Mylist.difference (Rcontext.getFreeVars c) (Rcontext.getFreeVars l)) in
        Cache.add_introduced_vars x iv ;
        iv
    ;;

let rewriteRule rewrite env e vars =
    let (REF num) = ExpIntern.intern_exp (isACorC env) e in
    let rules0 = (try Cache.get_rules num with Cache.NoEntry ->
            let fr = Disc.find (Env.isAorC env) (Env.getRules env) (ExpIntern.decode_exp e) in
            let fr2 = List.map (fun (x) -> let (REF e)=ExpIntern.intern_exp (Env.isACorC env) x in e) fr in
                Cache.add_rules num fr2 ;
                fr2
            ) in
        let rules1 = (try Cache.get_context_rules num (getContextList env) with Cache.NoEntry ->
            (*let _ = print_string ("Small for " ^ (prExp (ExpIntern.decode_exp e)) ^ "\n") in*)
            let rl = Disc.findSmall (Env.getContextDisc env) (ExpIntern.decode_exp e) in
            (*let _ = List.map (fun (x) -> print_string ("Rule: " ^ (prExp x) ^ "n")) rl in*)
            let rl1 = List.map (fun (x) -> let (REF e)=ExpIntern.intern_exp (isACorC env) x in e) rl in
                Cache.add_context_rules num (getContextList env) rl1 ;
                rl1
            ) in
        let rules = List.map (fun (x) -> (x,introduced_vars x)) (Mylist.remove_dups (rules1@rules0)) in
        let _ = Rtrace.trace_list "rewriteRuleDisc" (fun (x) -> List.map prExp (Env.getAllRules env)) in
        let _ = Rtrace.trace "rewriteRuleDisc" (fun (x) -> Disc.makedcstring (getRules env)) in
        let _ = Rtrace.trace "rewriteRule" (fun (x) -> "Rules:") in
        let _ = Rtrace.indent () in
        let _ = Rtrace.trace "rewriteRuleDisc" (fun (x) -> ("P: " ^ (Disc.makedcstring (getRules env)))) in
        let _ = Rtrace.trace_list "rewriteRule" (fun (xx) -> List.map (fun (x,_) -> prExp (ExpIntern.decode_exp (REF x))) rules) in
        let _ = Rtrace.undent () in
        let rec app_rule l = match l with
          | [] -> []
          | ((a,iv)::b) ->
            if iv=[] then
                let r = rewriteUsingRule rewrite env (ExpIntern.decode_exp e) vars (ExpIntern.decode_exp (REF a))
                in
                    if r=[] then
                        app_rule b
                    else
                        r
            else
                let r = rewriteUsingRuleUnify rewrite env (ExpIntern.decode_exp e) vars (ExpIntern.decode_exp (REF a))
                in
                    if r=[] then
                        app_rule b
                    else
                        r in
        let res = app_rule rules
    in
        res
    ;;

let forceRewrites rewrite env e =
    let rules = (Disc.find (Env.isAorC env) (Env.getViolationDisc env) e) in
    let _ = Rtrace.trace "rewriteRule" (fun (x) -> "Force rules") in
    let _ = Rtrace.trace_list "rewriteRuleDisc" (fun (x) -> List.map prExp (Env.getAllRules env)) in
    let _ = Rtrace.trace "rewriteRuleDisc" (fun (x) -> Disc.makedcstring (getRules env)) in
    let _ = Rtrace.trace "rewriteRule" (fun (x) -> "Rules:") in
    let _ = Rtrace.indent () in
    let _ = Rtrace.trace "rewriteRuleDisc" (fun (x) -> ("P: " ^ (Disc.makedcstring (Env.getRules env)))) in
    let _ = Rtrace.trace_list "rewriteRule" (fun (xx) -> List.map prExp rules) in
    let _ = Rtrace.undent () in
    let res = (List.fold_left List.append [] (List.map
                  (fun (r) -> List.map
                      (fun (a,b,c,d) -> (a,b,c,r))
                      (possibleRewritesUsingRule rewrite env e [] r))
                      rules)) in
        res
    ;;




