(******************************************************************************
 *                       
 * REWRITELIB
 *
 * kbrewrite.ml
 *
 * This file contains heuristic rewriting.
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
(* require "trace.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "env.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "context.sml" ;  *)
(* require "crewrite.sml" ;  *)
(* require "kbrewr-s.sml" ;  *)
(* require "bmf.sml" ;  *)
(* require "match.sml" ;  *)
(* require "basis.__list" ;  *)
(* require "basis.__integer" ;  *)

structure KBREWRITEimpl : KBREWRITE =
    struct
        open listimpl ;
        open Exp ;
        open MATCHimpl ;
        open TRACEimpl ;
        open EXP_INTERNimpl ;
        open INTERNimpl ;
        open ENVimpl ;
        open SUBSTimpl ;
        open CONTEXTimpl ;
        open CREWRITEimpl ;

        infix 2 *| ;
        infix 2 <| ;
        infix 2 |> ;
        infix 2 <> ;
        infix 3 >< ;

        fun member a nil = false
          | member a (f::r) = a=f orelse member a r

        structure DoubleIntDict = BinaryMapFn(DOUBLE_INT_KEY) ;
        structure IntDict = BinaryMapFn(INT_KEY) ;

        fun parent [x] = nil
          | parent nil = nil
          | parent (a::b) = a::(parent b)
          ;

        fun is_relevant_operator env (APPL (f,[a,b])) =
            (isEQ env f) orelse (isOrder env f)
          | is_relevant_operator env _ = false
          ;

        fun is_relevant_term env (APPL (17,[x])) =
            is_relevant_term env x
          | is_relevant_term env x = is_relevant_operator env x
          ;

        fun is_conjunction (APPL (9,l)) = true
          | is_conjunction _ = false
          ;

        fun is_useful_conjunction env (APPL (9,l)) =
            member true (map (is_relevant_term env) l)
          | is_useful_conjunction env _ = false
          ;

        fun conjunction_p exp nil = is_conjunction exp
          | conjunction_p exp x =
            (is_conjunction (getSubterm exp x)) orelse
                (case (getSubterm exp x)
                   of (APPL (17,[_])) =>
                      (is_conjunction (getSubterm exp (parent x)))
                    | _                  => false)
          ;

        fun default_operand (APPL (f,[APPL (t1,l1),APPL (t2,l2)])) =
            t1=71 orelse t1=72 orelse t2=t1 orelse t2=72
          | default_operand (APPL (f,[_,APPL (t,l)])) =
            t=71 orelse t=72
          | default_operand (APPL (f,[APPL (t,l),_])) =
            t=71 orelse t=72
          | default_operand _ = false

        fun useful_subterm env exp t =
            (is_useful_conjunction env (getSubterm exp t)) orelse
            ((is_relevant_operator env (getSubterm exp t)) andalso
             not(conjunction_p exp (parent t)) andalso
             not(default_operand (getSubterm exp t)))
          ;

        fun possible_terms env e =
            (allSubterms e) |> (useful_subterm env e)
          ;

        fun collect_terms env attrib f n =
            (selectAttrib env attrib f) |>
            (fn ((E e)::(E v)::_) =>
                (case e of
                    (APPL (ff,l)) => (f=ff) andalso ((n= ~1) orelse (isAC env f) orelse (isC env f) orelse (equal env (List.nth (l,n)) v))
                  | _             => false
                )
            )
          ;

        fun get_subst env p e =
            let val mat = match env p e |> (fn (x,y) => y=nil)
                val mat2 = map (fn (x,y) => x) mat
            in
                (hd mat2)
            end ;

        fun test_subterm_relationship rewrite env (APPL (f,l)) n um cm =
            let
                (*val _ = print ("[Testing " ^ (prExp (APPL (f,l))) ^ " " ^ (Int.toString n) ^ " " ^ um ^ " " ^ cm ^ "]\n")*)
                val mt_ops = (collect_terms env um f n) <>
                             (fn (x) => (case x
                                           of [_,_,(S oo)] => (oo,0)
                                            | [_,_,(S oo),(S eo)] => (oo,eo))
                             )
                (*val _ = print ("[" ^ (Int.toString (length mt_ops)) ^ "]\n")*)
                val ct_ops = (collect_terms env cm f n) |>
                             (fn (ct) =>
                                 (case (ct)
                                     of [(E e),_,_,(E c)] =>
                                         member
                                             (APPL (intern_true,nil))
                                             (rewrite
                                                 env
                                                 (subst
                                                     (get_subst env e (APPL (f,l)))
                                                     c
                                                 )
                                             )
                                      | [(E e),_,_,_,(E c)] =>
                                         member
                                             (APPL (intern_true,nil))
                                             (rewrite
                                                 env
                                                 (subst
                                                     (get_subst env e (APPL (f,l)))
                                                     c
                                                 )
                                             )
                                    | _                        => false
                                 )
                             ) <>
                             (fn (x) => (case x
                                           of [_,_,(S oo),_] => (oo,0)
                                            | [_,_,(S oo),(S eo),_] => (oo,eo))
                             )
            in
                remove_dups (mt_ops @ ct_ops)
            end ;
          ;

        fun upto (~1) = nil
          | upto 0 = [0]
          | upto n = n::(upto (n-1))
          ;

        fun get_term_attrib env (APPL (17,[x])) =
            get_term_attrib env x
          | get_term_attrib env (APPL (10,[a,b])) =
            let val (e1,o1,t1) = get_term_attrib env a
                val (e2,o2,t2) = get_term_attrib env b
            in
                if (e1=0) then
                    (e2,o2,t2)
                else
                    (e1,o1,t1)
            end
          | get_term_attrib env (APPL (f,l)) =
            (case getAttrib env intern_to [S(f)]
              of [S(f),S(e)] => (intern_to,f,e)
               | _ => (
                 case getAttrib env intern_po [S(f)]
                    of [S(f),S(e)] => (intern_po,f,e)
                     | _ => (
                      case getAttrib env intern_epo [S(f)]
                        of [S(f),S(e)] => (intern_epo,f,e)
                         | _ => (
                           case getAttrib env intern_eto [S(f)]
                             of [S(f),S(e)] => (intern_eto,f,e)
                              | _ => (0,0,0)
            ))))
          | get_term_attrib _ _ = (0,0,0)
          ;

        fun find_subterm nil nil v = v
          | find_subterm (a::b) (c::d) v =
            if a=v then c else find_subterm b d v
          ;
(*
        fun get_inc_onestep (APPL (f,l)) =
            let val tu = collect_terms env intern_omi f (~1)
                val tc = collect_terms env intern_cmi f (~1)
            in

        fun reduce_one_step env (APPL (f,[l,r]) =
            let
                val (t,oo,e) = get_term_attrib env (APPL (f,[l,r]))
                val (e,v) = if t=intern_po orelse t=intern_to then
                                get_onestep r
                            else
                                get_onestep l
            in
                if t=0 then
                    (APPL (f,l))
                else
            end ;
*)
        fun break_term rewrite env (APPL (f,l)) =
            let (*val _ = print ("[Break term " ^ (prExp (APPL (f,l))) ^ "]\n")*)
                val ops =
                    map (fn (n) => ((test_subterm_relationship
                                     rewrite env (APPL (f,l)) n intern_smi intern_csmi)@
                                    (test_subterm_relationship
                                     rewrite env (APPL (f,l)) n intern_omi intern_comi),
                                    (test_subterm_relationship
                                     rewrite env (APPL (f,l)) n intern_smd intern_csmd)@
                                    (test_subterm_relationship
                                     rewrite env (APPL (f,l)) n intern_omd intern_comd),
                                    (test_subterm_relationship
                                     rewrite env (APPL (f,l)) n intern_mi intern_cmi),
                                    (test_subterm_relationship
                                     rewrite env (APPL (f,l)) n intern_md intern_cmd)))
                    (upto ((length l)-1))
            in
                break_list rewrite env (APPL (f,l)) l ops
            end
          | break_term rewrite env (REF x) =
            break_term rewrite env (decode_one_exp (REF x))
          | break_term rewrite env x = nil
        and break_list rewrite env e nil nil = nil
          | break_list rewrite env e (a::b) ((nil,nil,nil,nil)::r) =
            break_list rewrite env e b r
          | break_list rewrite env e (a::b) ((smi,smd,mi,md)::r) =
            (break_term rewrite env a)@
            (map (fn (n,eq) => (APPL (n,[a,e]))) smi)@
            (map (fn (n,eq) => (APPL (n,[e,a]))) smd)@
            (map (fn (n,eq) => (APPL (intern_or,[APPL (eq,[a,e]),APPL (n,[a,e])]))) mi)@
            (map (fn (n,eq) => (APPL (intern_or,[APPL (eq,[e,a]),APPL (n,[e,a])]))) md)@
            (break_list rewrite env e b r)
          ;

        fun break_down_op rewrite env (APPL (f,[a,b])) =
            if is_relevant_operator env (APPL (f,[a,b])) then
                ((break_term rewrite env a)@(break_term rewrite env b))
            else
                nil
          | break_down_op rewrite env (APPL (17,[APPL (f,[a,b])])) =
            if is_relevant_operator env (APPL (f,[a,b])) then
                ((break_term rewrite env a)@(break_term rewrite env b))
            else
                nil
          | break_down_op rewrite env x = nil
          ;

        fun decode_term x =
            case decode_one_exp x
              of (APPL (17,[x])) => (APPL (17,[decode_term x]))
               | (APPL (9,[a,b])) => (APPL (9,[decode_term a,decode_term b]))
               | (APPL (10,[a,b])) => (APPL (10,[decode_term a,decode_term b]))
               | x => x

        fun break_down_ops rewrite env (APPL (9,l)) =
            foldr append nil (map (fn (x) => break_down_op rewrite env (decode_term x)) l)
          | break_down_ops rewrite env x = break_down_op rewrite env (decode_term x)

        fun binary_term (APPL (9,[a,b])) =
            (binary_term a) andalso (binary_term b)
          | binary_term (APPL (10,[a,b])) =
            (binary_term a) andalso (binary_term b)
          | binary_term (APPL (f,[a,b])) = true
          | binary_term (APPL (17,[a])) = binary_term a
          | binary_term _ = false
          ;

        fun get_env_terms env vars =
            let
                val rules = getContextList env <> (fn (x) => decode_exp (REF x))
                (*val _ = print "Rules:\n"
                val _ = map (fn (x) => print ("    rule " ^ (prExp x) ^ "\n")) rules*)
                val rules = rules |>
                    (fn (x) =>
                        (case x
                          of (APPL (1,[l,(APPL (4,nil)),(APPL (4,nil))])) => true
                           | (APPL (1,[l,(APPL (5,nil)),(APPL (4,nil))])) => true
                           | (APPL (1,[l,(APPL (17,[APPL (5,nil)])),(APPL (4,nil))])) => true
                           | (APPL (1,[l,(APPL (17,[APPL (4,nil)])),(APPL (4,nil))])) => true
                           | _ => false))
            in
                rules |> (fn (x) => not((intersect (getMarkedVars x) vars)=nil))
                      <> (fn (x) => (case x
                                       of (APPL (f,[l,(APPL (4,nil)),c])) => unmarkVars l
                                        | (APPL (f,[l,(APPL (5,nil)),c])) => (APPL (intern_not,[unmarkVars l]))
                                        | (APPL (f,[l,(APPL (17,[APPL (5,nil)])),c])) => unmarkVars l
                                        | (APPL (f,[l,(APPL (17,[APPL (4,nil)])),c])) => (APPL (17,[unmarkVars l]))))
                      |> binary_term
            end ;

        fun is_lt (t,oo,e) (APPL (9,[(APPL (f1,l1)),(APPL (17,[APPL (f2,l2)]))])) =
            (t=intern_eto orelse t=intern_epo) andalso f1=oo andalso f2=e
          | is_lt (t,oo,e) (APPL (9,[(APPL (17,[APPL (f2,l1)])),(APPL (f1,l2))])) =
            (t=intern_eto orelse t=intern_epo) andalso f1=oo andalso f2=e
          | is_lt (t,oo,e) (APPL (17,[APPL (f,l)])) =
            (t=intern_eto andalso oo=f)
          | is_lt (t,oo,e) (APPL (f,l)) =
            (t=intern_to orelse t=intern_po) andalso f=oo
          | is_lt _ _ = false
          ;

        fun mk_lt (t,oo,e) l r =
            if t=intern_eto then
                (APPL (intern_not,[APPL (oo,[r,l])]))
            else if t=intern_epo then
                (APPL (intern_and,[APPL (oo,[l,r]),APPL (intern_not,[APPL (e,[l,r])])]))
            else
                (APPL (oo,[l,r]))
          ;

        fun is_le (t,oo,e) (APPL (10,[APPL (f1,l1),APPL (f2,l2)])) =
            (t=intern_to orelse t=intern_po) andalso ((f1=oo andalso f2=e) orelse (f1=e andalso f2=oo))
          | is_le (t,oo,e) (APPL (17,[APPL (f,l)])) =
            (t=intern_to andalso f=oo)
          | is_le (t,oo,e) (APPL (f,l)) =
            (t=intern_eto orelse t=intern_epo) andalso f=oo
          | is_le _ _ = false
          ;

        fun mk_le (t,oo,e) l r =
            if t=intern_to then
                (APPL (intern_not,[APPL (oo,[r,l])]))
            else if t=intern_po then
                (APPL (intern_or,[APPL (oo,[l,r]),APPL (e,[l,r])]))
            else
                (APPL (oo,[l,r]))
          ;

        fun is_eq (t,oo,e) (APPL (f,l)) = e=f
          | is_eq _ _ = false
          ;

        fun mk_eq (t,oo,e) l r = (APPL (e,[l,r]))
          ;

        fun is_ne (t,oo,e) (APPL (17,[APPL (f,l)])) =
            e=f
          | is_ne _ _ = false
          ;

        fun mk_ne (t,oo,e) l r = (APPL (intern_not,[APPL (e,[l,r])]))
          ;

        fun get_left_operand _ (APPL (f,nil)) = (REF 0)
          | get_left_operand (e,oo,t) (APPL (9,[APPL (f,l),a])) =
            if f=e then
                get_left_operand (e,oo,t) a
            else
                get_left_operand (e,oo,t) (APPL (f,l))
          | get_left_operand (e,oo,t) (APPL (10,[APPL (f,l),a])) =
            if f=e then
                get_left_operand (e,oo,t) a
            else
                get_left_operand (e,oo,t) (APPL (f,l))
          | get_left_operand oper (APPL (17,[x])) = get_right_operand oper x
          | get_left_operand _ (APPL (f,[a,b])) = a
          (*| get_left_operand _ x = (print ((prExp x) ^ "\n") ; x)*)
        and get_right_operand _ (APPL (f,nil)) = (REF 0)
          | get_right_operand (e,oo,t) (APPL (9,[APPL (f,l),a])) =
            if f=e then
                get_right_operand (e,oo,t) a
            else
                get_right_operand (e,oo,t) (APPL (f,l))
          | get_right_operand (e,oo,t) (APPL (10,[APPL (f,l),a])) =
            if f=e then
                get_right_operand (e,oo,t) a
            else
                get_right_operand (e,oo,t) (APPL (f,l))
          | get_right_operand oper (APPL (17,[x])) = get_left_operand oper x
          | get_right_operand _ (APPL (f,[a,b])) = b
          (*| get_right_operand _ x = (print ((prExp x) ^ "\n") ; x)*)
          ;

        fun intern_term env (APPL (17,[x])) =
            (APPL (17,[intern_term env x]))
          | intern_term env (APPL (9,[a,b])) =
            (APPL (10,[intern_term env a,intern_term env b]))
          | intern_term env (APPL (10,[a,b])) =
            (APPL (9,[intern_term env a,intern_term env b]))
          | intern_term env (REF x) = intern_term env (decode_one_exp (REF x))
          | intern_term env x =
            decode_one_exp
                (intern_exp (isACorC env) x)

        fun and_combine_operands oper op1 op2 =
            let val l1 = get_left_operand oper op1
                val r1 = get_right_operand oper op1
                val l2 = get_left_operand oper op2
                val r2 = get_right_operand oper op2
            in
                if is_eq oper op1 then
                   (if is_ne oper op2 then
                        (APPL (intern_false,nil))
                    else if is_le oper op2 then
                        op1
                    else if is_lt oper op2 then
                        (APPL (intern_false,nil))
                    else
                        op1)
                else if is_ne oper op1 then
                   (if is_ne oper op2 then
                        op1
                    else if is_eq oper op2 then
                        (APPL (intern_false,nil))
                    else if is_le oper op2 then
                        mk_lt oper l2 r2
                    else
                        op2)
                else if is_lt oper op1 then
                   (if is_ne oper op2 then
                        op1
                    else if is_eq oper op2 then
                        (APPL (intern_false,nil))
                    else if is_lt oper op2 then
                       (if l1=l2 then
                            op1
                        else
                            (APPL (intern_false,nil)))
                    else
                       (if l1=l2 then
                            op1
                        else
                            (APPL (intern_false,nil))))
                else
                   (if is_ne oper op2 then
                        mk_lt oper l1 r1
                    else if is_eq oper op2 then
                        op2
                    else if is_lt oper op2 then
                       (if l1=l2 then
                            op2
                        else
                            (APPL (intern_false,nil)))
                    else
                       (if l1=l2 then
                            op1
                        else
                            mk_eq oper l1 r1))
            end ;

        fun same_operands oper op1 op2 =
            ((get_left_operand oper op1=get_left_operand oper op2) andalso
             (get_right_operand oper op1=get_right_operand oper op2)) orelse
            ((get_left_operand oper op1=get_right_operand oper op2) andalso
             (get_right_operand oper op1=get_left_operand oper op2))
          ;

        fun can_combine oper op1 op2 =
            (is_le oper op1 orelse is_lt oper op1 orelse is_eq oper op1 orelse is_ne oper op1) andalso
            (is_le oper op2 orelse is_lt oper op2 orelse is_eq oper op2 orelse is_ne oper op2) andalso
            same_operands oper op1 op2
          ;

        fun elaborate_phase env oper terms =
            let (*val _ = map (fn (x) => print ("t = " ^ (prExp (intern_term env x)) ^ "\n")) terms*)
                val terms = map (intern_term env) terms
                (*val _ = print "Combine\n"
                val _ = flush_out std_out*)
                fun add_terms oo t nil = (oo,t)
                  | add_terms oo t (f::rest) =
                    let val f = intern_term env f
                        (*val _ = print ("t1 = " ^ (prExp f) ^ "\n")*)
                        val (REF l) = get_left_operand oper f
                        val (REF r) = get_right_operand oper f
                        val (REF n) = intern_exp (isACorC env) f
                        val lv = case (IntDict.find (oo,l))
                                   of SOME x => x
                                    | NONE   => nil
                        val rv = case (IntDict.find (oo,r))
                                   of SOME x => x
                                    | NONE   => nil
                        val dv = case DoubleIntDict.find (t,(l,r))
                                   of SOME x => x
                                    | NONE   => nil
                        val nlv = if member n lv then lv else n::lv
                        val nrv = if member n rv then rv else n::rv
                        val ndv = if member n dv then dv else n::dv
                    in
                        add_terms
                            (IntDict.insert (IntDict.insert (oo,l,nlv),r,nrv))
                            (DoubleIntDict.insert (t,(l,r),ndv))
                            rest
                    end
                fun delete_terms oo t nil = (oo,t)
                  | delete_terms oo t (f::rest) =
                    let val f = intern_term env f
                        (*val _ = print ("t2 = " ^ (prExp f) ^ "\n")*)
                        val (REF l) = get_left_operand oper f
                        val (REF r) = get_right_operand oper f
                        val (REF n) = intern_exp (isACorC env) f
                        val lv = case (IntDict.find (oo,l))
                                   of SOME x => x
                                    | NONE   => nil
                        val rv = case (IntDict.find (oo,r))
                                   of SOME x => x
                                    | NONE   => nil
                        val dv = case DoubleIntDict.find (t,(l,r))
                                   of SOME x => x
                                    | NONE   => nil
                        val nlv = if member n lv then delete n lv else lv
                        val nrv = if member n rv then delete n rv else rv
                        val ndv = if member n dv then delete n dv else dv
                    in
                        delete_terms
                            (IntDict.insert (IntDict.insert (oo,l,nlv),r,nrv))
                            (DoubleIntDict.insert (t,(l,r),ndv))
                            rest
                    end
                val (one_ref,two_ref) = add_terms 
                            (IntDict.empty : int list IntDict.map)
                            (DoubleIntDict.empty : int list DoubleIntDict.map) terms
                (*val _ = print "Combine\n"
                val _ = flush_out std_out*)
                fun combine_all [x] = x
                  | combine_all (f::r) =
                    and_combine_operands oper f (combine_all r)
                fun combine_terms one_ref two_ref =
                    DoubleIntDict.foldli (fn ((f,s),nl,(st,dt)) =>
                        if length nl < 2 then
                            (st,dt)
                        else let val terms = nl <> (fn (x) => decode_term (REF x))
                                 val (REF new) = intern_exp (isACorC env) (combine_all terms)
                                 val (st,dt) = delete_terms st dt (nl <> (fn (x) => REF x))
                                 val (st,dt) = add_terms st dt [(REF new)]
                             in
                                 (st,dt)
                             end) (one_ref,two_ref) two_ref
                val (one_ref,two_ref) = combine_terms one_ref two_ref
                (*val _ = print "Loop\n"
                val _ = flush_out std_out*)
                (*val _ = trace_list "kbrewrite" (fn (e) => map (fn (x,_) => ("start " ^ (prExp x))) terms)
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x,_) => ("combined " ^ (prExp x))) combined)*)

                fun make_tran_op1 make op1 op2 =
                    let (*val _ = print ("t3 = " ^ (prExp op1) ^ "\n")
                        val _ = print ("t4 = " ^ (prExp op2) ^ "\n")*)
                        val l1 = get_left_operand oper op1
                        val l2 = get_left_operand oper op2
                        val r1 = get_right_operand oper op1
                        val r2 = get_right_operand oper op2
                    in
                        (if l1=l2 then [make oper r2 r1] else nil) @
                        (if l1=r2 then [make oper l2 r1] else nil) @
                        (if r1=l2 then [make oper l1 r2] else nil) @
                        (if r1=r2 then [make oper l1 l2] else nil)
                    end ;

                fun make_tran_op2 make op1 op2 =
                    let val l1 = get_left_operand oper op1
                        val l2 = get_left_operand oper op2
                        val r1 = get_right_operand oper op1
                        val r2 = get_right_operand oper op2
                    in
                        (if l1=l2 then [make oper r1 r2] else nil) @
                        (if l1=r2 then [make oper l2 r1] else nil) @
                        (if r1=l2 then [make oper l1 r2] else nil) @
                        (if r1=r2 then [make oper l2 l1] else nil)
                    end ;

                fun make_dir_tran_op make op1 op2 =
                    let val l1 = get_left_operand oper op1
                        val l2 = get_left_operand oper op2
                        val r1 = get_right_operand oper op1
                        val r2 = get_right_operand oper op2
                    in
                        (if l1=r2 then [make oper l2 r1] else nil) @
                        (if r1=l2 then [make oper l1 r2] else nil)
                    end ;

                fun closure_terms op1 op2 =
                    if is_eq oper op1 then
                       (if is_eq oper op2 then
                            make_tran_op2 mk_eq op1 op2
                        else if is_ne oper op2 then
                            make_tran_op2 mk_ne op1 op2
                        else if is_lt oper op2 then
                            make_tran_op2 mk_lt op1 op2
                        else if is_le oper op2 then
                            make_tran_op2 mk_le op1 op2
                        else
                            nil)
                   else if is_ne oper op1 then
                       (if is_eq oper op2 then
                            make_tran_op2 mk_ne op1 op2
                        else
                            nil)
                   else if is_lt oper op1 then
                       (if is_eq oper op2 then
                            make_tran_op1 mk_lt op1 op2
                        else if is_lt oper op2 then
                            make_dir_tran_op mk_lt op1 op2
                        else if is_le oper op2 then
                            make_dir_tran_op mk_lt op1 op2
                        else
                            nil)
                   else if is_le oper op1 then
                       (if is_eq oper op2 then
                            make_tran_op1 mk_lt op1 op2
                        else if is_lt oper op2 then
                            make_dir_tran_op mk_lt op1 op2
                        else if is_le oper op2 then
                            make_dir_tran_op mk_le op1 op2
                        else
                            nil)
                   else
                       nil
                fun transitive_cycle _ nil = nil
                  | transitive_cycle op1 (op2::r) =
                    (closure_terms op1 op2) @ (transitive_cycle op1 r)
                fun new_transitive_terms sd dd terms =
                    foldr append nil (map (fn (term) =>
                         let val (REF l) = get_left_operand oper term
                             val (REF r) = get_right_operand oper term
                             val SOME t1 = IntDict.find (sd,l)
                             val SOME t2 = IntDict.find (sd,r)
                             val t = t1@t2
                             val terms = map (fn (x) => decode_term (REF x)) t
                         in
                             transitive_cycle term terms
                         end) terms)
                  ;

                fun transitive_closure sd dd terms =
                    let
                        (*val _ = trace_list "kbrewrite" (fn (e) => map (fn (x,_) => ("l " ^ (prExp x))) l)*)
                        fun represented_by _ (APPL (5,nil)) = true
                          | represented_by (APPL (5,nil)) _ = false
                          | represented_by op1 op2 =
                            (same_operands oper op1 op2) andalso
                            (((is_ne oper op1) andalso ((is_ne oper op2) orelse (is_lt oper op2))) orelse
                             ((is_lt oper op1) andalso (is_lt oper op2)) orelse
                             ((is_eq oper op1) andalso (is_eq oper op2)) orelse
                             ((is_le oper op1) andalso ((is_le oper op2) orelse (is_lt oper op2) orelse (is_eq oper op2)))) ;
                        fun member x dd =
                            let val (REF left) = get_left_operand oper x
                                val (REF right) = get_right_operand oper x
                                val nl1 = case DoubleIntDict.find (dd,(left,right))
                                            of SOME x => x
                                             | NONE => nil
                                val nl2 = case DoubleIntDict.find (dd,(right,left))
                                            of SOME x => x
                                             | NONE => nil
                                val nl = nl1@nl2
                                val terms = nl <> (fn (x) => decode_term (REF x))
                            in
                                not((terms |> (fn (t) => represented_by x t))=nil)
                            end
                        val nt = new_transitive_terms sd dd terms
                        (*val _ = print "    Cycle\n"
                        val _ = map (fn (x) => print ("    b " ^ (prExp x) ^ "\n")) nt*)
                        val nt1 = nt <> (intern_exp (isACorC env)) <> (fn (REF x) => x)
                        val ntdict = foldr (fn (e,d) => IntDict.insert (d,e,true)) (IntDict.empty) nt1
                        val nt = (IntDict.listItems ntdict) <> (fn (a,b) => intern_term env (REF a))
                        val new_trans = nt |> (fn (x) => not(member x dd))
                        (*val _ = map (fn (x) => print ("    a " ^ (prExp x) ^ "\n")) new_trans*)
                    in
                        if new_trans=nil then
                            (sd,dd)
                        else
                            let val (sd,dd) = add_terms sd dd new_trans
                                (*val res = fold append (map (fn (_,t) => t) (DoubleIntDict.listItems dd)) nil*)
                                (*val _ = map (fn (x) => print ("    t1 = " ^ (prExp (decode_term (REF x))) ^ "\n")) res*)
                                val (sd,dd) = combine_terms sd dd
                                (*val res = fold append (map (fn (_,t) => t) (DoubleIntDict.listItems dd)) nil*)
                                (*val _ = map (fn (x) => print ("    t2 = " ^ (prExp (decode_term (REF x))) ^ "\n")) res*)
                            in
                                transitive_closure sd dd new_trans
                            end
                    end
                val (sd,dd) = transitive_closure one_ref two_ref terms
                val res = foldr append nil (map (fn (_,t) => t) (DoubleIntDict.listItems dd))
                (*val _ = map (fn (x) => print ("    t = " ^ (prExp (decode_term (REF x))) ^ "\n")) res*)
            in
                res <> (fn (x) => decode_term (REF x))
            end ;

        val has_change = ref false ;

        fun rewrite_op (t,c,e) env cond_terms terms =
            let val _ = trace "kbrewrite" (fn (x) => ("Operator " ^ (decode t) ^ " " ^ (decode c) ^ " " ^ (decode e)))
                val terms = map (intern_term env) terms
                (*val _ = print "elaborate1\n"
                val _ = flush_out std_out*)
                val results1 = elaborate_phase env (t,c,e) cond_terms
                (*val _ = print "elaborate2\n"
                val _ = flush_out std_out*)
                val results2 = elaborate_phase env (t,c,e) (terms@cond_terms)
                (*val _ = print "elaborate3\n"
                val _ = flush_out std_out*)
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("results1 " ^ (prExp x))) results1)
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("results2 " ^ (prExp x))) results2)
                fun strengthen_term nil term = term
                  | strengthen_term (a::b) term =
                    if (can_combine (t,c,e) a term) andalso not(a=term) then
                        (has_change := true ; a)
                    else
                        strengthen_term b term
            in
                if member (APPL (intern_false,nil)) results1 then
                    (has_change := true ; [[APPL (intern_true,nil)]])
                else if member (APPL (intern_false,nil)) results2 then
                    (has_change := true ; [[APPL (intern_false,nil)]])
                else
                    [terms <> (strengthen_term results2) |>
                     (fn (x) => if member x results1 then (if member x terms then ((has_change := true) ; false) else false) else true)]
            end ;

        fun make_builtins ((NUM n)::r) =
            (make_num_builtins n r)@(make_builtins r)
          | make_builtins ((STRING s)::r) =
            (make_string_builtins s r)@(make_builtins r)
          | make_builtins ((CHAR c)::r) =
            (make_char_builtins c r)@(make_builtins r)
          | make_builtins (f::r) = make_builtins r
          | make_builtins nil = nil
        and make_num_builtins n1 ((NUM n2)::r) =
            if n1<n2 then
                (APPL (intern_less,[NUM n1,NUM n2]))::(APPL (intern_preceq,[NUM n1,NUM n2]))::(make_num_builtins n1 r)
            else if n2<n1 then
                (APPL (intern_less,[NUM n2,NUM n1]))::(APPL (intern_preceq,[NUM n2,NUM n1]))::(make_num_builtins n1 r)
            else
                make_num_builtins n1 r
          | make_num_builtins n (_::r) = make_num_builtins n r
          | make_num_builtins _ nil = nil
        and make_string_builtins n1 ((STRING n2)::r) =
            if n1<n2 then
                (APPL (intern_preceq,[STRING n1,STRING n2]))::(make_string_builtins n1 r)
            else if n2<n1 then
                (APPL (intern_preceq,[STRING n2,STRING n1]))::(make_string_builtins n1 r)
            else
                make_string_builtins n1 r
          | make_string_builtins n (_::r) = make_string_builtins n r
          | make_string_builtins _ nil = nil
        and make_char_builtins n1 ((CHAR n2)::r) =
            if n1<n2 then
                (APPL (intern_preceq,[CHAR n1,CHAR n2]))::(make_char_builtins n1 r)
            else if n2<n1 then
                (APPL (intern_preceq,[CHAR n2,CHAR n1]))::(make_char_builtins n1 r)
            else
                make_char_builtins n1 r
          | make_char_builtins n (_::r) = make_char_builtins n r
          | make_char_builtins _ nil = nil

        fun get_constants (NUM n) = [NUM n]
          | get_constants (STRING s) = [STRING s]
          | get_constants (CHAR c) = [CHAR c]
          | get_constants (APPL (f,l)) =
            foldr append nil (map get_constants l)
          | get_constants (REF x) = get_constants (decode_exp (REF x))
          | get_constants _ = nil

        fun main_rewrite_loop rewrite env exp =
            let
                val _ = trace "kbrewrite" (fn (xx) => "KB Rewriting " ^ (prExp exp))
                val _ = indent ()
                val terms = break_down_ops rewrite env exp |> binary_term
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("term " ^ (prExp x))) terms)
                val env_terms = get_env_terms env (getFreeVars (decode_exp exp))
                (*val _ = map (fn (x) => print ("env " ^ (prExp x) ^ "\n")) env_terms*)
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("env_term " ^ (prExp x))) env_terms)
                val env_op_terms =
                    foldr append nil (map (break_down_ops rewrite env) env_terms) |>
                         binary_term
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("env_op_term " ^ (prExp x))) env_op_terms)
                val cond_terms = terms@env_terms@env_op_terms
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("cond_term " ^ (prExp x))) cond_terms)
                val exp_terms1 = (case exp
                                  of (APPL (9,l)) => l
                                   | _              => [exp])
                val exp_terms = exp_terms1 |> binary_term
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("exp_term " ^ (prExp x))) exp_terms)
                val all_terms = (exp_terms@cond_terms)
                val operands = foldr append nil (map get_constants all_terms)
                val builtin_terms = make_builtins operands
                val cond_terms = cond_terms@builtin_terms
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("cond_term " ^ (prExp x))) cond_terms)
                val residue = difference exp_terms1 exp_terms
                val _ = trace_list "kbrewrite" (fn (e) => map (fn (x) => ("residue " ^ (prExp x))) residue)
                val ops = remove_dups
                           (map (get_term_attrib env) (terms@env_terms@exp_terms))
                          |> (fn (t,_,_) => not(t=0))
                val _ = (has_change := false)
                val res = map (fn (x) => (case (x@residue)
                                            of [a] => a
                                             | nil => (APPL (intern_true,nil))
                                             | _   => (APPL (intern_and,(x@residue))))
                              )
                              (foldr
                                  (fn (oper,expl) =>
                                      foldr append nil (map (rewrite_op oper env cond_terms) expl)
                                  )
                                  [exp_terms] ops)
                val _ = trace "kbrewrite" (fn (xx) => "Results: " ^ (prExp exp))
                val _ = indent ()
                val _ = trace_list "kbrewrite" (fn (xx) => map prExp res)
                val _ = undent ()
                val _ = undent ()
            in
                if (!has_change) then res else nil
            end ;

        fun rewritexx rewrite env e =
            let val res = rewrite env e in
                if res=nil then
                    [e]
                else
                    res
            end ;

        fun build_env rewrite env e nil = env
          | build_env rewrite env e (a::b) =
            build_env rewrite (create_rules (rewritexx rewrite env) env e a) (getSubterm e [a]) b
          ;

        fun prIlist nil = ""
          | prIlist [a] = Int.toString (a:int)
          | prIlist (a::b) = (Int.toString a) ^ "," ^ (prIlist b)

        fun make_cond_env rewrite env e sub =
            let
                (*val _ = print ("[creating env " ^ (prExp e) ^ " " ^ (prIlist sub) ^ "]\n")*)
                val env1 = build_env rewrite env e sub
            in
                env1
            end ;

        val in_kb = ref false ;

        fun kbrewrite rewrite e x t =
            let val _ = (in_kb := true)
                (*val _ = print "start kbrewrite\n"
                val _ = TextIO.flushOut TextIO.stdOut*)
                val r = map (replaceSubterm x t) (main_rewrite_loop rewrite (make_cond_env rewrite e x t) (getSubterm x t))
                        |> (fn (xx) => not(equal e x xx))
                (*val _ = print "finish kbrewrite\n"
                val _ = TextIO.flushOut TextIO.stdOut*)
                val _ = (in_kb := false)
            in
                r
            end
          ;

        fun kbrewrite2 rewrite e x t =
            if (!in_kb) then
                nil
            else
                let val _ = (in_kb := true)
                    (*val _ = print ("start kbrewrite " ^ (prExp (decode_exp x)) ^ "\n")
                    val _ = TextIO.flushOut TextIO.stdOut*)
                    val r = map (replaceSubterm x t) (main_rewrite_loop rewrite (make_cond_env rewrite e x t) (getSubterm x t))
                            |> (fn (xx) => equal_smaller e xx x)
                    (*val _ = print "finish kbrewrite\n"
                    val _ = TextIO.flushOut TextIO.stdOut*)
                    val _ = (in_kb := false)
                in
                    r
                end
          ;

    end ;



