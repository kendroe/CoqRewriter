(******************************************************************************
 *                          
 * REWRITELIB           
 *  
 * inner.ml  
 *             
 * This file contains the code for the inner rewrite routines.
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
(* require "intern.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "kbrewrit.sml" ;  *)
(* require "context.sml" ;  *)
(* require "derive.sml" ;  *)
(* require "builtin.sml" ;  *)
(* require "match.sml" ;  *)
(* require "trace.sml" ;  *)
(* require "cache.sml" ;  *)
(* require "crewrite.sml" ;  *)
(* require "rule_app.sml" ;  *)
(* require "rewrit-s.sml" ;  *)
(* require "basis.__list" ;  *)

(* open listimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

open Exp ;;
(* open ENVimpl ; *)
open Intern ;;
(* open EXP_INTERNimpl ; *)
(* open KBREWRITEimpl ; *)
(* open CONTEXTimpl ; *)
(* open DERIVEimpl ; *)
(* open BUILTINimpl ; *)
(* open MATCHimpl ; *)
(* open TRACEimpl ; *)
(* open CACHEimpl ; *)
(* open CREWRITEimpl ; *)
(* open RULE_APPimpl ; *)

let replace_appl_n (APPL (f,l)) n a = (APPL (f,Mylist.replace_nth l n a)) ;;

let rec rewrite_list rewrite env l = match l with
  | [] -> [[]]
  | (a::b) ->
    (List.map
        (fun (x,y) -> x::y)
        (List.combine (rewrite env a) (rewrite_list rewrite env b))) ;;

let rec remove_normals e = match e with
  | (NORMAL x) -> x
  | (APPL (f,l)) -> (APPL (f,List.map remove_normals l))
  | y -> y ;;

let rec mark_vars env x =
   (try let (REF n) = ExpIntern.intern_exp (Env.isACorC env) x in
        let res = Cache.get_mark_vars n in
        (REF res)
    with Cache.NoEntry ->
    let (REF n) = ExpIntern.intern_exp (Env.isACorC env) x in
    let (REF res) = ExpIntern.intern_exp (Env.isACorC env) (Rcontext.markVars (ExpIntern.decode_exp x)) in
         Cache.add_mark_vars n res ;
         (REF res)
    ) ;;

let generate_rules r env e = match r with
  | 9 -> (List.filter
             (fun (r) -> (match (ExpIntern.decode_exp r) with
                      | (APPL (f,[APPL (4,[]);_;c])) -> false
                      | (APPL (f,[APPL (5,[]);_;c])) -> false
                      | (APPL (1,[l;r;c])) -> not(l=r)
                      | _ -> false))
       (List.map remove_normals
           (Derive.derive env (ExpIntern.intern_exp (Env.isACorC env) (APPL (intern_oriented_rule,[mark_vars env (remove_normals e);(APPL (intern_true,[]));(APPL (intern_true,[]))]))))))
  |10 ->
       (List.filter
           (fun (r) -> (match (ExpIntern.decode_exp r) with
                      | (APPL (f,[APPL (4,[]);_;c])) -> false
                      | (APPL (f,[APPL (5,[]);_;c])) -> false
                      | (APPL (1,[l;r;c])) -> true
                      | _ -> false))
           (List.map remove_normals
               (Derive.derive env (ExpIntern.intern_exp (Env.isACorC env) (APPL (intern_oriented_rule,[mark_vars env e;(APPL (intern_false,[]));(APPL (intern_true,[]))])))))) ;;

let rec generate_rules_list f env l = match l with
  | [] -> []
  | (a::b) ->
    (generate_rules f env a)::(generate_rules_list f env b)
  ;;

let rec add_c_rules env l = match l with
  | [] -> env
  | (a::b) ->
    add_c_rules (Env.addProperty env a) b
  ;;

let rec add_context_rules env l n term =
    (*let _ = print_string ("Adding " ^ (string_of_int n) ^ "\n") in*)
    let l = Mylist.delete_nth l n in
    (*let _ = List.map (fun x -> (List.map (fun y -> print_string ("Rule " ^ (prExp (ExpIntern.decode_exp y)) ^ "\n")) x)) l in*)
        (*addContextRules env (List.map (fun (x) -> (REF x)) (junction_filter_rule_list env term (List.map (fun (x) -> let val REF x = ExpIntern.intern_exp (Env.isACorC env) x in x end ) (foldr append [] l))))*)
        Env.addContextRules env (List.fold_right List.append l [])
  ;;

let rec context_rewrite_list rewrite env f rules l n =
    if (List.length l)=n then
        ((l,env),rules)
    else
        let t = List.nth l n in
        let (x,_) = rewrite (t,(add_context_rules env rules n t)) in
        let gr = if Match.equal env x t then List.nth rules n else (generate_rules f env x) in
            context_rewrite_list rewrite env f
                (Mylist.replace_nth rules n gr)
                (Mylist.replace_nth l n x)
                (n+1)
  ;;

let rec repeat_context_rewrite_list rewrite env f rules l =
    let ((l2,env),new_rules) = context_rewrite_list rewrite env f rules l 0
    in
        if l2=l then
            (APPL (f,l2),env)
        else
            repeat_context_rewrite_list rewrite env f new_rules l2
    ;;

let rec repeat_rewrite rewrite ((APPL (f,l)),env) =
    let rules = generate_rules_list f env l in
    let (r,env) = repeat_context_rewrite_list rewrite env f rules l in
        (Env.flatten_top env r,env)
    ;;

exception NoRewrite ;;

let intern_exp_true = ExpIntern.intern_exp (fun (x) -> false) (parseExp "True") ;;
let intern_exp_false = ExpIntern.intern_exp (fun (x) -> false) (parseExp "False") ;;

let flt (e,env) = (Env.flatten_top env e,env) ;;

let kbmode = ref true ;;

let rec rewriteTop (exp,env) kb =
    (let _ = Rtrace.trace "rewrite" (fun (x) -> "builtin " ^ (prExp exp)) in
    let _ = Rtrace.indent () in
    let exp = ExpIntern.decode_two_exp (ExpIntern.intern_exp (Env.isACorC env) exp) in
    let rew = Builtin.builtin rewrite2_front env exp in
    (*let _ = print_string ("exp = " ^ (prExp exp) ^ "\n") in
    let _ = print_string ("builtin results " ^ (string_of_int (List.length rew)) ^ "\n") in*)
    let _ = Rtrace.trace_list "rewrite" (fun (x) -> List.map prExp rew) in
    let _ = Rtrace.undent () in
    let _ = Rtrace.trace "rewrite" (fun (x) -> "end builtin " ^ (prExp (List.hd (rew@[exp])))) in
    let (res,env,kb) =
            (if rew = [] || rew=[exp] then
                let x = (Rule_app.rewriteRule rewrite2_front env exp []) in
                    if x=[] || x==[exp] then
                       (if (!kbmode) && kb && (Kbrewrite.useful_subterm env exp []) then
                            let n = Kbrewrite.kbrewrite2 rewrite2_front env (ExpIntern.decode_two_exp exp) []
                            in
                                if n=[] then raise NoRewrite
                                else (List.hd n,env,false)
                        else
                            raise NoRewrite)
                    else (List.hd x,env,kb)
            else 
                (List.hd rew,env,kb))
    in
        (Env.flatten env res,env,kb))
and rewriteTopCont orig (f,env) kb =
   (try let (x,env,kb) = rewriteTop (f,env) kb
        (*val _ = print ("[Pre: " ^ (prExp f) ^ "]\n[Post: " ^ (prExp x) ^ "]\n")*)
    in
        if (ExpIntern.intern_exp (Env.isACorC env) orig)=(ExpIntern.intern_exp (Env.isACorC env) x) then
            (x,env)
        else
            try if kb then
                rewrite_front (x,env)
            else
                rewrite_nokb_front (x,env)
            with NoRewrite -> (x,env)
    with NoRewrite -> (f,env))
and int_rewrite e kb = (match e with
  | ((APPL (18,[c;e1;e2])),env) ->
    (let (x,env) = rewrite_front (ExpIntern.decode_two_exp c,env) in
    let res = if x=intern_exp_true then
                      rewrite_front (ExpIntern.decode_two_exp e1,env)
                  else if x=intern_exp_false then
                      rewrite_front (e2,env)
                  else
                      let env1 = Crewrite.create_rules (fun (x) -> [remove_normals x]) env (APPL (intern_if,[x;e1;e2])) 1 in
                      let (res1,env1b) = rewrite_front (e1,env1) in
                      let env2 = Crewrite.create_rules (fun (x) -> [remove_normals x]) env (APPL (intern_if,[x;e1;e2])) 2 in
                      let (res2,env2b) = rewrite_front (e2,env2) in
                          rewriteTopCont (APPL (18,[c;e1;e2])) ((APPL (intern_if,[x;res1;res2])),env) kb
    in
        res)
  | ((APPL (9,[])),env) -> (intern_exp_true,env)
  | ((APPL (9,[(APPL (17,[x]))])),env) ->
    if (!kbmode) && kb && Kbrewrite.useful_subterm env x [] then
        let (x,_) = rewrite_front (x,env) in
        let x = List.hd ((Kbrewrite.kbrewrite2 rewrite2_front env x [])@[x]) in
            rewriteTopCont (APPL (9,[(APPL (17,[x]))])) (APPL (intern_not,[x]),env) kb
    else
        let (x,_) = rewrite_front (x,env)
        in
            rewriteTopCont (APPL (9,[(APPL (17,[x]))])) (APPL (intern_not,[x]),env) kb
  | ((APPL (9,[x])),env) ->
    if (!kbmode) && kb && Kbrewrite.useful_subterm env x [] then
        let (x,_) = rewrite_front (x,env) in
        let x = List.hd ((Kbrewrite.kbrewrite2 rewrite2_front env x [])@[x]) in
            rewriteTopCont (APPL (9,[x])) (x,env) kb
    else
        rewrite_front (x,env)
  | (APPL (9,l),env) ->
    rewriteTopCont (APPL (9,l)) (flt (repeat_rewrite rewrite_nokb_front ((APPL (intern_and,l)),env))) kb
  | ((APPL (10,[])),env) -> (intern_exp_false,env)
  | ((APPL (10,[x])),env) -> rewrite_front (ExpIntern.intern_exp (Env.isACorC env) x,env)
  | (APPL (10,l),env) ->
    rewriteTopCont (APPL (10,l)) (flt (repeat_rewrite rewrite_front ((APPL (intern_or,l)),env))) kb
  | (APPL (1,[l;r;c]),env) ->
    let (c,env) = rewrite_front (c,env) in
    let e2 = Crewrite.create_rules (fun (x) -> [remove_normals x]) env
                 (APPL (intern_oriented_rule,[l;r;c])) 0 in
    let _ = Rtrace.trace_list "rewrite" (fun (x) -> List.map (fun (x) -> "contadd " ^ (prExp (ExpIntern.decode_exp (REF x)))) (Env.getContextList e2)) in
    let (l,_) = rewrite_front (l,e2) in
    let (r,_) = rewrite_front (r,e2) in
        rewriteTopCont (APPL (1,[l;r;c])) (APPL (intern_oriented_rule,[l;r;c]),env) kb
  | (APPL (2,[l;r;c]),env) ->
    let (c,env) = rewrite_front (c,env) in
    let e2 = Crewrite.create_rules (fun (x) -> [remove_normals x]) env
                 (APPL (intern_unoriented_rule,[l;r;c])) 0 in
    let (l,_) = rewrite_front (l,e2) in
    let (r,_) = rewrite_front (r,e2) in
        rewriteTopCont (APPL (2,[l;r;c])) (APPL (intern_unoriented_rule,[l;r;c]),env) kb
  | (APPL (17,[e]),env) ->
    let (e2,envs) = if kb then rewrite_sub env e 0 else rewrite_nokb_sub env e 0 in
    let (res,env) = rewriteTopCont (APPL (17,[e])) ((APPL (intern_not,[e2])),envs) kb in
        (res,env)
  | (APPL (71,l),env) -> rewriteTopCont (APPL (71,l)) (APPL (71,l),env) kb
  | (APPL (72,l),env) -> rewriteTopCont (APPL (72,l)) (APPL (72,l),env) kb
  | (APPL (s,l2),env) ->
    let (l,envs,_) = List.fold_left (fun (r,env,n) -> (fun e ->
                                let (e,env) = (rewrite_sub env e n)
                                in
                                   (r@[e],env,n+1)
                                )) ([],env,0) l2 in
    let (res,env) = rewriteTopCont (APPL (s,l2)) ((APPL (s,l)),envs) kb in
        if l=l2 then (Env.flatten_top env res,env) else (res,env)
  | ((QUANT (v,t,e,p)),env) ->
    let (r_e,env) = try rewrite_front (e,env) with NoRewrite -> (e,env) in
    let (r_p,env) = try rewrite_front (p,env) with NoRewrite -> (p,env) in
        rewriteTopCont (QUANT (v,t,e,p)) ((QUANT (v,t,r_e,r_p)),env) kb
  | (LET (v,t,e,p),env) ->
    let (r_e,env) = rewrite_front (e,env) in
    let (r_p,env) = rewrite_front (p,env) in
        rewriteTopCont (LET (v,t,e,p)) (LET (v,t,r_e,r_p),env) kb
  | (CASE (e,t,c),env) ->
    let (r_e,env) = rewrite_front (e,env) in
    let r_c = List.map (fun (p,e) -> (p,let (e,_) = rewrite_front (e,env) in e)) c in
        rewriteTopCont (CASE (e,t,c)) (CASE (r_e,t,r_c),env) kb
  | (x,env) -> rewriteTopCont x (x,env) kb)
and rewrite_sub env x n = try rewrite_front (x,env) with NoRewrite -> (x,env)
and rewrite_nokb_sub env x n = rewrite_nokb_front (x,env)
and rewrite2_front env x =
    (let _ = Rtrace.trace "rewrite" (fun (xx) -> "rewriting subterm: " ^ (prExp x)) in
    let _ = Rtrace.indent () in
        (*val _ = print ("[Rewriting " ^ (prExp x) ^ "]\n")*)
        (*val _ = flush_out std_out*)
    let (res1,_) = rewrite_front (x,env) in
    let res = [res1] in
        (*val _ = print ("[    Result " ^ (prExp res1) ^ "]\n")*)
        (*val _ = flush_out std_out*)
    let _ = Rtrace.trace "rewrite" (fun (x) -> "Result:") in
    let _ = Rtrace.indent () in
    let _ = Rtrace.trace_list "rewrite" (fun (x) -> List.map prExp res) in
    let _ = Rtrace.trace_list "rewrite" (fun (x) -> List.map (fun (x) -> prExp (ExpIntern.decode_exp x)) res) in
    let _ = (Rtrace.undent () ; Rtrace.undent ()) in
    let _ = Rtrace.trace "rewrite" (fun (xx) -> "end rewrite subterm: " ^ (prExp x)) in
    let _ = (Rtrace.indent () ; Rtrace.undent ()) in
        List.map (ExpIntern.intern_exp (Env.isACorC env)) res)
and rewrite2 env x = (Cache.save_good_rewrites () ; [(ExpIntern.decode_exp (List.hd (rewrite2_front env x)))])
and rewrite_front_k (x,env) kb =
    let _ = Rtrace.trace "rewrite"
                (fun (xx) -> "rewriting expression: " ^ (prExp x)) in
    let _ = Rtrace.indent () in
    let (REF x1) = ExpIntern.intern_exp (Env.isACorC env) x in
    let (res1,env) =
           if Cache.is_rewrite_failure (Env.getContextList env) x1 then
               (REF x1,env)
           else
               (try ((REF (Cache.get_rewrite (Env.getContextList env) x1)),env)
                with Cache.NoEntry ->
                let x = ExpIntern.decode_two_exp (REF x1) in
                let (r,env) = int_rewrite (x,env) kb in
                let (REF res) = ExpIntern.intern_exp (Env.isACorC env) r in
                    if x1=res then
                        Cache.add_rewrite_failure (Env.getContextList env) x1
                    else
                        Cache.add_rewrite (Env.getContextList env) x1 res ;
                    ((REF res),env)
                ) in
    let _ = Rtrace.trace "rewrite" (fun (x) -> "Result:") in
    let _ = Rtrace.indent () in
    let _ = Rtrace.trace "rewrite" (fun (x) -> prExp res1) in
    let _ = Rtrace.trace "rewrite" (fun (x) -> prExp (ExpIntern.decode_exp res1)) in
    let _ = (Rtrace.undent () ; Rtrace.undent ()) in
    let _ = Rtrace.trace "rewrite" (fun (xx) -> "end rewrite for: " ^ (prExp x)) in
    let _ = (Rtrace.indent () ; Rtrace.undent ()) in
        (ExpIntern.intern_exp (Env.isACorC env) res1,env)
and rewrite_front (x,env) =
    let _ = Rtrace.trace "rewrite"
                (fun (xx) -> "rewriting expression: " ^ (prExp x)) in
    let _ = Rtrace.indent () in
    let (REF x1) = ExpIntern.intern_exp (Env.isACorC env) x in
    let (res1,env) =
           if Cache.is_rewrite_failure (Env.getContextList env) x1 then
               (REF x1,env)
           else
               (try ((REF (Cache.get_rewrite (Env.getContextList env) x1)),env)
                with Cache.NoEntry ->
                let x = ExpIntern.decode_two_exp (REF x1) in
                let (r,env) = int_rewrite (x,env) true in
                let (REF res) = ExpIntern.intern_exp (Env.isACorC env) r in
                    if x1=res then
                        Cache.add_rewrite_failure (Env.getContextList env) x1
                    else
                        Cache.add_rewrite (Env.getContextList env) x1 res ;
                    ((REF res),env)
                ) in
    let _ = Rtrace.trace "rewrite" (fun (x) -> "Result:") in
    let _ = Rtrace.indent () in
    let _ = Rtrace.trace "rewrite" (fun (x) -> prExp res1) in
    let _ = Rtrace.trace "rewrite" (fun (x) -> prExp (ExpIntern.decode_exp res1)) in
    let _ = (Rtrace.undent () ; Rtrace.undent ()) in
    let _ = Rtrace.trace "rewrite" (fun (xx) -> "end rewrite for: " ^ (prExp x)) in
    let _ = (Rtrace.indent () ; Rtrace.undent ())
    in
        (ExpIntern.intern_exp (Env.isACorC env) res1,env)
and rewrite_nokb_front (x,env) =
    let _ = Rtrace.trace "rewrite"
                (fun (xx) -> "rewriting expression: " ^ (prExp x)) in
    let _ = Rtrace.indent () in
    let (REF x1) = ExpIntern.intern_exp (Env.isACorC env) x in
    let (res1,env) =
           if Cache.is_rewrite_failure (Env.getContextList env) x1 then
               (REF x1,env)
           else
               (try ((REF (Cache.get_rewrite (Env.getContextList env) x1)),env)
                with Cache.NoEntry ->
                let x = ExpIntern.decode_two_exp (REF x1) in
                let (r,env) = int_rewrite (x,env) false in
                let (REF res) = ExpIntern.intern_exp (Env.isACorC env) r in
                    if x1=res then
                        Cache.add_rewrite_failure (Env.getContextList env) x1
                    else
                        Cache.add_rewrite (Env.getContextList env) x1 res ;
                    ((REF res),env)
                ) in
    let _ = Rtrace.trace "rewrite" (fun (x) -> "Result:") in
    let _ = Rtrace.indent () in
    let _ = Rtrace.trace "rewrite" (fun (x) -> prExp res1) in
    let _ = (Rtrace.undent () ; Rtrace.undent ()) in
    let _ = Rtrace.trace "rewrite" (fun (xx) -> "end rewrite for: " ^ (prExp x)) in
    let _ = (Rtrace.indent () ; Rtrace.undent ()) in
        (ExpIntern.intern_exp (Env.isACorC env) res1,env)
and rewrite_nokb (x,env) =
    let _ = Cache.save_good_rewrites () in
    let (x,env) = rewrite_nokb_front (x,env) in
        (ExpIntern.decode_exp x,env)
and rewrite (x,env) =
    let _ = Cache.save_good_rewrites () in
    let (x,env) = rewrite_front (x,env) in
        (ExpIntern.decode_exp x,env)
    ;;

let rewrite_in_context (e,c,d,env) =
    let e2 = Crewrite.create_rules (fun (x) -> [remove_normals x]) env
                 (APPL (intern_unoriented_rule,[e;(APPL (intern_true,[]));c])) 0 in
        (*val _ = print ("d(1) = " ^ (prExp d) ^ "\n")*)
    let (d,_) = rewrite_front (d,e2) in
        (*val _ = print ("d(2) = " ^ (prExp d) ^ "\n")*)
    let e3 = Crewrite.create_rules (fun (x) -> [remove_normals x]) e2
                 (APPL (intern_unoriented_rule,[e;(APPL (intern_true,[]));d])) 0 in
        (*val _ = print ("e(1) = " ^ (prExp e) ^ "\n")*)
    let (e,_) = rewrite_front (e,e3) in
        (*val _ = print ("e(2) = " ^ (prExp (ExpIntern.decode_exp e)) ^ "\n")*)
        (*val _ = map (fun (x) -> print ("r " ^ (prExp (ExpIntern.decode_exp (REF x))) ^ "\n")) (Env.getContextList e3)*)
    let e = List.hd ((Kbrewrite.kbrewrite2 rewrite2 e3 (ExpIntern.decode_exp e) [])@[e])
        (*val _ = print ("e(3) = " ^ (prExp (ExpIntern.decode_exp e)) ^ "\n")*)
    in
        e
    ;;

