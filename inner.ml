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

let replace_appl_n (APPL (f,l)) n a = (APPL (f,replace_nth (l,n,a))) ;;

let rec rewrite_list rewrite env l = match l with
  | [] -> [[]]
  | (a::b) ->
    (List.map
        (fun (x,y) -> x::y)
        ((rewrite env a) >< (rewrite_list rewrite env b))) ;;

let rec remove_normals e = match e with
  | (NORMAL x) -> x
  | (APPL (f,l)) -> (APPL (f,map remove_normals l))
  | y -> y ;;

let rec mark_vars env x =
   (try let (REF n) = ExpIntern.intern_exp (Env.isACorC env) x in
        let res = Cache.get_mark_vars n in
        (REF res)
    with Cache.NoEntry ->
    let val (REF n) = ExpIntern.intern_exp (isACorC env) x
        val (REF res) = ExpIntern.intern_exp (isACorC env) (markVars (ExpIntern.decode_exp x))
    in
         Cache.add_mark_vars n res ;
         (REF res)
    end)

let generate_rules r env e = match r with
  | 9 -> (List.filter
             (fun (r) -> (match (ExpIntern.decode_exp r) with
                      | (APPL (f,[APPL (4,[]);_;c])) -> false
                      | (APPL (f,[APPL (5,[]);_;c])) -> false
                      | (APPL (1,[l;r;c])) -> not(l=r)
                      | _ -> false))
       (List.map remove_normals
           (Derive.derive env (ExpIntern.intern_exp (isACorC env) (APPL (intern_oriented_rule,[mark_vars env (remove_normals e),(APPL (intern_true,[])),(APPL (intern_true,[]))])))))
  | 10 ->
       (derive env (ExpIntern.intern_exp (isACorC env) (APPL (intern_oriented_rule,[mark_vars env e,(APPL (intern_false,[])),(APPL (intern_true,[]))]))))
    <> remove_normals
    |> (fn (r) => (case (ExpIntern.decode_exp r)
                     of (APPL (f,[APPL (4,[]),_,c])) => false
                      | (APPL (f,[APPL (5,[]),_,c])) => false
                      | (APPL (1,[l,r,c])) => true
                      | _ => false))

fun generate_rules_list f env [] = []
  | generate_rules_list f env (a::b) =
    (generate_rules f env a)::(generate_rules_list f env b)
  ;

fun add_c_rules env [] = env
  | add_c_rules env (a::b) =
    add_c_rules (addProperty env a) b
  ;

fun add_context_rules env l n term =
    let val l = delete_nth (l,n)
    in
        (*addContextRules env (map (fn (x) => (REF x)) (junction_filter_rule_list env term (map (fn (x) => let val REF x = ExpIntern.intern_exp (isACorC env) x in x end ) (foldr append [] l))))*)
        addContextRules env (foldr append [] l)
    end
  ;

fun context_rewrite_list rewrite env f rules l n =
    if (length l)=n then
        ((l,env),rules)
    else
        let
            val t = List.nth (l,n)
            val (x,_) = rewrite (t,(add_context_rules env rules n t))
            val gr = if equal env x t then List.nth (rules,n) else generate_rules f env x
        in
            context_rewrite_list rewrite env f
                (replace_nth (rules,n,gr))
                (replace_nth (l,n,x))
                (n+1)
        end
  ;

fun repeat_context_rewrite_list rewrite env f rules l =
    let val ((l2,env),new_rules) = context_rewrite_list rewrite env f rules l 0
    in
        if l2=l then
            (APPL (f,l2),env)
        else
            repeat_context_rewrite_list rewrite env f new_rules l2
    end ;

fun repeat_rewrite rewrite ((APPL (f,l)),env) =
    let
        val rules = generate_rules_list f env l
        val (r,env) = repeat_context_rewrite_list rewrite env f rules l
    in
        (flatten_top env r,env)
    end

exception NoRewrite ;

val intern_exp_true = ExpIntern.intern_exp (fn (x) => false) (parseExp "True")
val intern_exp_false = ExpIntern.intern_exp (fn (x) => false) (parseExp "False")

fun flt (e,env) = (flatten_top env e,env)

val kbmode = ref false

fun rewriteTop (exp,env) kb =
    let val _ = trace "rewrite" (fn (x) => "builtin " ^ (prExp exp)) ;
        val _ = indent () ;
        val exp = decode_two_exp (ExpIntern.intern_exp (isACorC env) exp)
        val rew = builtin rewrite2_front env exp ;
        val _ = trace_list "rewrite" (fn (x) => map prExp rew) ;
        val _ = undent () ;
        val _ = trace "rewrite" (fn (x) => "end builtin " ^ (prExp exp)) ;
        val (res,env,kb) =
            if rew = [] then
                let val x = (rewriteRule rewrite2_front env exp [])
                in
                    if x=[] then
                       (if (!kbmode) andalso kb andalso (useful_subterm env exp []) then
                            let val n = kbrewrite2 rewrite2_front env (decode_two_exp exp) []
                            in
                                if n=[] then raise NoRewrite
                                else (hd n,env,false)
                            end
                        else
                            raise NoRewrite)
                    else (hd x,env,kb)
                end
            else 
                (hd rew,env,kb)
    in
        (flatten env res,env,kb)
    end
and rewriteTopCont (f,env) kb =
   (let val (x,env,kb) = rewriteTop (f,env) kb
        (*val _ = print ("[Pre: " ^ (prExp f) ^ "]\n[Post: " ^ (prExp x) ^ "]\n")*)
    in
        if kb then
            rewrite_front (x,env)
        else
        rewrite_nokb_front (x,env)
    end handle NoRewrite => (f,env))
and int_rewrite ((APPL (18,[c,e1,e2])),env) kb =
    let val (x,env) = rewrite_front (decode_two_exp c,env)
        val res = if x=intern_exp_true then
                      rewrite_front (decode_two_exp e1,env)
                  else if x=intern_exp_false then
                      rewrite_front (e2,env)
                  else
                      let val env1 = create_rules (fn (x) => [remove_normals x]) env (APPL (intern_if,[x,e1,e2])) 1
                          val (res1,env1b) = rewrite_front (e1,env1)
                          val env2 = create_rules (fn (x) => [remove_normals x]) env (APPL (intern_if,[x,e1,e2])) 2
                          val (res2,env2b) = rewrite_front (e2,env2)
                      in
                          rewriteTopCont ((APPL (intern_if,[x,res1,res2])),env) kb
                      end
    in
        res
    end
  | int_rewrite ((APPL (9,[])),env) kb = (intern_exp_true,env)
  | int_rewrite ((APPL (9,[(APPL (17,[x]))])),env) kb =
    if (!kbmode) andalso kb andalso useful_subterm env x [] then
        let val (x,_) = rewrite_front (x,env)
            val x = hd ((kbrewrite2 rewrite2_front env x [])@[x])
        in
            rewriteTopCont (APPL (intern_not,[x]),env) kb
        end
    else
        let val (x,_) = rewrite_front (x,env)
        in
            rewriteTopCont (APPL (intern_not,[x]),env) kb
        end
  | int_rewrite ((APPL (9,[x])),env) kb =
    if (!kbmode) andalso kb andalso useful_subterm env x [] then
        let val (x,_) = rewrite_front (x,env)
            val x = hd ((kbrewrite2 rewrite2_front env x [])@[x])
        in
            rewriteTopCont (x,env) kb
        end
    else
        rewrite_front (x,env)
  | int_rewrite (APPL (9,l),env) kb =
    rewriteTopCont (flt (repeat_rewrite rewrite_nokb_front ((APPL (intern_and,l)),env))) kb
  | int_rewrite ((APPL (10,[])),env) kb = (intern_exp_false,env)
  | int_rewrite ((APPL (10,[x])),env) kb = rewrite_front (ExpIntern.intern_exp (isACorC env) x,env)
  | int_rewrite (APPL (10,l),env) kb =
    rewriteTopCont (flt (repeat_rewrite rewrite_front ((APPL (intern_or,l)),env))) kb
  | int_rewrite (APPL (1,[l,r,c]),env) kb =
    let val (c,env) = rewrite_front (c,env)
        val e2 = create_rules (fn (x) => [remove_normals x]) env
                 (APPL (intern_oriented_rule,[l,r,c])) 0
        val _ = trace_list "rewrite" (fn (x) => map (fn (x) => "contadd " ^ (prExp (ExpIntern.decode_exp (REF x)))) (getContextList e2))
        val (l,_) = rewrite_front (l,e2)
        val (r,_) = rewrite_front (r,e2)
    in
        rewriteTopCont (APPL (intern_oriented_rule,[l,r,c]),env) kb
    end
  | int_rewrite (APPL (2,[l,r,c]),env) kb =
    let val (c,env) = rewrite_front (c,env)
        val e2 = create_rules (fn (x) => [remove_normals x]) env
                 (APPL (intern_unoriented_rule,[l,r,c])) 0
        val (l,_) = rewrite_front (l,e2)
        val (r,_) = rewrite_front (r,e2)
    in
        rewriteTopCont (APPL (intern_unoriented_rule,[l,r,c]),env) kb
    end
  | int_rewrite (APPL (17,[e]),env) kb =
    let val (e2,envs) = if kb then rewrite_sub env e 0 else rewrite_nokb_sub env e 0
        val (res,env) = rewriteTopCont ((APPL (intern_not,[e2])),envs) kb
    in
        (res,env)
    end
  | int_rewrite (APPL (71,l),env) kb = rewriteTopCont (APPL (71,l),env) kb
  | int_rewrite (APPL (72,l),env) kb = rewriteTopCont (APPL (72,l),env) kb
  | int_rewrite (APPL (s,l2),env) kb =
    let val (l,envs,_) = foldl (fn (e,(r,env,n)) =>
                                let val (e,env) = (rewrite_sub env e n)
                                in
                                   (r@[e],env,n+1)
                                end) ([],env,0) l2
        val (res,env) = rewriteTopCont ((APPL (s,l)),envs) kb
    in
        if l=l2 then (flatten_top env res,env) else (res,env)
    end
  | int_rewrite ((QUANT (v,t,e,p)),env) kb =
    let val (r_e,env) = rewrite_front (e,env)
        val (r_p,env) = rewrite_front (p,env)
    in
        rewriteTopCont ((QUANT (v,t,r_e,r_p)),env) kb
    end
  | int_rewrite (LET (v,t,e,p),env) kb =
    let val (r_e,env) = rewrite_front (e,env) ;
        val (r_p,env) = rewrite_front (p,env) ;
    in
        rewriteTopCont (LET (v,t,r_e,r_p),env) kb
    end
  | int_rewrite (CASE (e,t,c),env) kb =
    let val (r_e,env) = rewrite_front (e,env) ;
        val r_c = map (fn (p,e) => (p,let val (e,_) = rewrite_front (e,env) in e end)) c ;
    in
        rewriteTopCont (CASE (r_e,t,r_c),env) kb
    end
  | int_rewrite x kb = rewriteTopCont x kb
and rewrite_sub env x n = rewrite_front (x,env)
and rewrite_nokb_sub env x n = rewrite_nokb_front (x,env)
and rewrite2_front env x =
    let val _ = trace "rewrite" (fn (xx) => "rewriting subterm: " ^ (prExp x))
        val _ = indent ()
        (*val _ = print ("[Rewriting " ^ (prExp x) ^ "]\n")*)
        (*val _ = flush_out std_out*)
        val (res1,_) = rewrite_front (x,env)
        val res = [res1]
        (*val _ = print ("[    Result " ^ (prExp res1) ^ "]\n")*)
        (*val _ = flush_out std_out*)
        val _ = trace "rewrite" (fn (x) => "Result:")
        val _ = indent ()
        val _ = trace_list "rewrite" (fn (x) => map prExp res)
        val _ = trace_list "rewrite" (fn (x) => map (fn (x) => prExp (ExpIntern.decode_exp x)) res)
        val _ = (undent () ; undent ())
        val _ = trace "rewrite" (fn (xx) => "end rewrite subterm: " ^ (prExp x))
        val _ = (indent () ; undent ()) ;
    in
        map (ExpIntern.intern_exp (isACorC env)) res
    end
and rewrite2 env x = (save_good_rewrites () ; [(ExpIntern.decode_exp (hd (rewrite2_front env x)))])
and rewrite_front_k (x,env) kb =
    let val _ = trace "rewrite"
                (fn (xx) => "rewriting expression: " ^ (prExp x))
        val _ = indent ()
        val (REF x1) = ExpIntern.intern_exp (isACorC env) x
        val (res1,env) =
           if is_rewrite_failure (getContextList env) x1 then
               (REF x1,env)
           else
               (((REF (get_rewrite (getContextList env) x1)),env)
                handle NoEntry =>
                let val x = decode_two_exp (REF x1)
                    val (r,env) = int_rewrite (x,env) kb
                    val (REF res) = ExpIntern.intern_exp (isACorC env) r
                in
                    if x1=res then
                        add_rewrite_failure (getContextList env) x1
                    else
                        add_rewrite (getContextList env) x1 res ;
                    ((REF res),env)
                end)
        val _ = trace "rewrite" (fn (x) => "Result:")
        val _ = indent ()
        val _ = trace "rewrite" (fn (x) => prExp res1)
        val _ = trace "rewrite" (fn (x) => prExp (ExpIntern.decode_exp res1))
        val _ = (undent () ; undent ())
        val _ = trace "rewrite" (fn (xx) => "end rewrite for: " ^ (prExp x)) ;
        val _ = (indent () ; undent ())
    in
        (ExpIntern.intern_exp (isACorC env) res1,env)
    end
and rewrite_front (x,env) =
    let val _ = trace "rewrite"
                (fn (xx) => "rewriting expression: " ^ (prExp x))
        val _ = indent ()
        val (REF x1) = ExpIntern.intern_exp (isACorC env) x
        val (res1,env) =
           if is_rewrite_failure (getContextList env) x1 then
               (REF x1,env)
           else
               (((REF (get_rewrite (getContextList env) x1)),env)
                handle NoEntry =>
                let val x = decode_two_exp (REF x1)
                    val (r,env) = int_rewrite (x,env) true
                    val (REF res) = ExpIntern.intern_exp (isACorC env) r
                in
                    if x1=res then
                        add_rewrite_failure (getContextList env) x1
                    else
                        add_rewrite (getContextList env) x1 res ;
                    ((REF res),env)
                end)
        val _ = trace "rewrite" (fn (x) => "Result:")
        val _ = indent ()
    val _ = trace "rewrite" (fn (x) => prExp res1)
        val _ = trace "rewrite" (fn (x) => prExp (ExpIntern.decode_exp res1))
        val _ = (undent () ; undent ())
        val _ = trace "rewrite" (fn (xx) => "end rewrite for: " ^ (prExp x)) ;
        val _ = (indent () ; undent ())
    in
        (ExpIntern.intern_exp (isACorC env) res1,env)
    end
and rewrite_nokb_front (x,env) =
    let val _ = trace "rewrite"
                (fn (xx) => "rewriting expression: " ^ (prExp x))
        val _ = indent ()
        val (REF x1) = ExpIntern.intern_exp (isACorC env) x
        val (res1,env) =
           if is_rewrite_failure (getContextList env) x1 then
               (REF x1,env)
           else
               (((REF (get_rewrite (getContextList env) x1)),env)
                handle NoEntry =>
                let val x = decode_two_exp (REF x1)
                    val (r,env) = int_rewrite (x,env) false
                    val (REF res) = ExpIntern.intern_exp (isACorC env) r
                in
                    if x1=res then
                        add_rewrite_failure (getContextList env) x1
                    else
                        add_rewrite (getContextList env) x1 res ;
                    ((REF res),env)
                end)
        val _ = trace "rewrite" (fn (x) => "Result:")
        val _ = indent ()
        val _ = trace "rewrite" (fn (x) => prExp res1)
        val _ = (undent () ; undent ())
        val _ = trace "rewrite" (fn (xx) => "end rewrite for: " ^ (prExp x)) ;
        val _ = (indent () ; undent ())
    in
        (ExpIntern.intern_exp (isACorC env) res1,env)
    end
and rewrite_nokb (x,env) =
    let val _ = save_good_rewrites ()
        val (x,env) = rewrite_nokb_front (x,env)
    in
        (ExpIntern.decode_exp x,env)
    end
and rewrite (x,env) =
    let val _ = save_good_rewrites ()
        val (x,env) = rewrite_front (x,env)
    in
        (ExpIntern.decode_exp x,env)
    end

fun rewrite_in_context (e,c,d,env) =
    let val e2 = create_rules (fn (x) => [remove_normals x]) env
                 (APPL (intern_unoriented_rule,[e,(APPL (intern_true,[])),c])) 0
        (*val _ = print ("d(1) = " ^ (prExp d) ^ "\n")*)
        val (d,_) = rewrite_front (d,e2)
        (*val _ = print ("d(2) = " ^ (prExp d) ^ "\n")*)
        val e3 = create_rules (fn (x) => [remove_normals x]) e2
                 (APPL (intern_unoriented_rule,[e,(APPL (intern_true,[])),d])) 0
        (*val _ = print ("e(1) = " ^ (prExp e) ^ "\n")*)
        val (e,_) = rewrite_front (e,e3)
        (*val _ = print ("e(2) = " ^ (prExp (ExpIntern.decode_exp e)) ^ "\n")*)
        (*val _ = map (fn (x) => print ("r " ^ (prExp (ExpIntern.decode_exp (REF x))) ^ "\n")) (getContextList e3)*)
        val e = hd ((kbrewrite2 rewrite2 e3 (ExpIntern.decode_exp e) [])@[e])
        (*val _ = print ("e(3) = " ^ (prExp (ExpIntern.decode_exp e)) ^ "\n")*)
    in
        e
    end

