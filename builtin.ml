(******************************************************************************
 *                       
 * REWRITELIB
 *          
 * builtin.ml 
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
(* require "env.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "type.sml" ;  *)
(* require "match.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "context.sml" ;  *)
(* require "trace.sml" ;  *)
(* require "built-s.sml" ;  *)
(* require "basis.__char" ;  *)
(* require "crewrite.sml" ;  *)

(* open listimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

(* open ENVimpl ; *)
open Exp ;;
open Intern ;;
(* open EXP_INTERNimpl ; *)
(* open TYPEimpl ; *)
(* open MATCHimpl ; *)
(* open SUBSTimpl ; *)
(* open CONTEXTimpl ; *)
(* open TRACEimpl ; *)

let rec and_remove l = match l with
  | [] -> []
  | ((APPL (4,[]))::r) -> and_remove r
  | (f::r) ->
    if List.mem f r then
        and_remove r
    else
        f::(and_remove r) ;;

let rec or_remove l = match l with
  | [] -> []
  | ((APPL (5,[]))::r) -> or_remove r
  | (f::r) ->
    if List.mem f r then
        or_remove r
    else
        f::(or_remove r) ;;

let rec pairJoin l1 l2 = match (l1,l2) with
  | ([],[]) -> []
  | ((a::b),(c::d)) -> (APPL (intern_equal,[NORMAL a;NORMAL c]))::pairJoin b d ;;

let rec precPairJoin l1 l2 = match (l1,l2) with
  | ([],[]) -> []
  | ((a::b),(c::d)) -> (APPL (intern_preceq,[NORMAL a;NORMAL c]))::precPairJoin b d ;;

let is_constructor x = ((decode x) >= "A" && (decode x) < "a") ;;

let rec lpFactor l t = match l with
  | [] -> []
  | (a::b) -> (APPL (intern_preceq,[NORMAL a;NORMAL t]))::(lpFactor b t) ;;

let rec spFactor t l = match l with
  | [] -> []
  | (a::b) -> (APPL (intern_preceq,[NORMAL t;NORMAL a]))::(spFactor t b) ;;

let rec member_eq env x l = match l with
  | [] -> false
  | (a::b) ->
    (Match.equal env x a) || (member_eq env x b) ;;

let rec delete_eq env x l = match l with
  | [] -> []
  | (a::b) ->
    if Match.equal env x a then b else a::(delete_eq env x b) ;;

let rec flatten_constructors l = match l with
  | [] -> []
  | ((REF x)::r) ->
    flatten_constructors ((ExpIntern.decode_one_exp (REF x))::r)
  | ((APPL (s,l1))::l2) ->
    if is_constructor s then
        (flatten_constructors l1)@(flatten_constructors l2)
    else
        (APPL (s,l1))::(flatten_constructors l2)
  | (a::l) -> a::(flatten_constructors l)
  ;;

let rec e_member env x l = match l with
  | [] -> false
  | (f::r) ->
    (ExpIntern.intern_exp (Env.isACorC env) x)=(ExpIntern.intern_exp (Env.isACorC env) f) ||
    e_member env x r ;;

exception NoProper ;;

let rec first_proper l = match l with
  | [] -> raise NoProper
  | ((a,[])::_) -> a
  | ((a,_)::r) -> first_proper r
  ;;

let rec subterm_member env a e = match e with
  | (REF x) ->
    subterm_member env a (ExpIntern.decode_one_exp (REF x))
  | (APPL (s,l)) ->
    (Match.equal env a (APPL (s,l))) ||
    (if is_constructor s then
        subterm_member_list env a l
     else
        false)
  | b -> Match.equal env a b
and subterm_member_list env a l = match l with
  | [] -> false
  | (b::c) ->
    (subterm_member env a b) || (subterm_member_list env a c)
  ;;

let bool_type = Type.parse "Bool" ;;

let rec returns_bool env e = match e with
  | (QUANT (14,_,_,_)) -> true
  | (QUANT (15,_,_,_)) -> true
  | (APPL (s,l)) ->
    Type.getReturnetype (Env.getType env s) = bool_type
  | _ -> false
  ;;

let rec strip l = match l with
  | [] -> []
  | ((a,b)::r) -> a::(strip r) ;;

let rec unstrip l = match l with
  | [] -> []
  | (a::b) -> (a,Type.notype)::(unstrip b) ;;

let rec find_equal vars l l2 = match l2 with
  | [] -> NOEXP
  | ((REF x)::r) -> find_equal vars l ((ExpIntern.decode_two_exp (REF x))::r)
  | ((APPL (11,[REF v1;VAR v2]))::r) ->
    find_equal vars l ((APPL (11,[ExpIntern.decode_one_exp (REF v1);VAR v2]))::r)
  | ((APPL (11,[REF v1;REF v2]))::r) ->
    find_equal vars l ((APPL (11,[ExpIntern.decode_one_exp (REF v1);ExpIntern.decode_one_exp (REF v2)]))::r)
  | ((APPL (11,[VAR v1;REF v2]))::r) ->
    find_equal vars l ((APPL (11,[VAR v1;ExpIntern.decode_one_exp (REF v2)]))::r)
  | ((APPL (11,[VAR v1;VAR v2]))::r) ->
    if List.mem v1 vars && not(v1=v2) then
        (APPL (intern_equal,[VAR v1;VAR v2]))
    else if List.mem v2 vars && not(v1=v2) then
        (APPL (intern_equal,[VAR v1;VAR v2]))
    else
        find_equal vars ((APPL (intern_equal,[VAR v1;VAR v2]))::l) r
  | ((APPL (11,[VAR v;x]))::r) ->
    if List.mem v vars && not(List.mem v (Context.getFreeVars x)) then
        (APPL (intern_equal,[VAR v;x]))
    else
        find_equal vars ((APPL (intern_equal,[VAR v;x]))::l) r
  | ((APPL (11,[x;VAR v]))::r) ->
    if List.mem v vars && not(List.mem v (Context.getFreeVars x)) then
        (APPL (intern_equal,[x;VAR v]))
    else
        find_equal vars ((APPL (intern_equal,[x;VAR v]))::l) r
  | (a::b) -> find_equal vars (a::l) b
  ;;

let rec make_subst vars e = match e with
  | (APPL (11,[VAR v1;VAR v2])) ->
    if List.mem v1 vars then
        Subst.addPair Subst.empty v1 (VAR v2)
    else
        Subst.addPair Subst.empty v2 (VAR v1)
  | (APPL (11,[VAR v;x])) ->
    Subst.addPair Subst.empty v x
  | (APPL (11,[x;VAR v])) ->
    Subst.addPair Subst.empty v x
  ;;

let rec gcd x1 x2 =
    if x1=0 || x2=0 then 0
    else if x1<0 then gcd (0-x1) x2
    else if x2<0 then gcd x1 (0-x2)
    else if (x1 > 0 && x1 < x2) || (x2 < 0 && x2 < x1) then
        let diff = x2 mod x1
        in
            if diff=0 then x1 else gcd x1 diff
    else
        let diff = x1 mod x2
        in
            if diff=0 then x2 else gcd x2 diff ;;

let rec rat_plus (RATIONAL (a,b)) (RATIONAL (c,d)) =
    let n = a*d+b*c in
    let d = b*d in
    let g = gcd n d in
    let g = if n=0 then d else if d<0 then 0-g else g in
        (RATIONAL (n / g,d / g))
    ;;

let rat_minus (RATIONAL (a,b)) (RATIONAL (c,d)) =
    let n = a*d-b*c in
    let d = b*d in
    let g = gcd n d in
    let g = if n=0 then d else if d<0 then 0-g else g in
        (RATIONAL (n / g,d / g))
    ;;

let rat_times (RATIONAL (a,b)) (RATIONAL (c,d)) =
    let n = a*c in
    let d = b*d in
    let g = gcd n d in
    let g = if n=0 then d else if d<0 then 0-g else g in
        (RATIONAL (n / g,d / g))
    ;;

let rat_divide (RATIONAL (a,b)) (RATIONAL (c,d)) =
    let n = a*d in
    let d = b*c in
    let g = gcd n d in
    let g = if n=0 then d else if d<0 then 0-g else g in
        (RATIONAL (n / g,d / g))
    ;;

let rec find_delete_encode_num l = match l with
  | [] -> (0,[])
  | (f::r) ->
    let x = ExpIntern.decode_exp f
    in
        match x with
           | (NUM n) -> (n,r)
           | _ -> let (n,l) = find_delete_encode_num r
                  in (n,f::l)
    ;;

let rec find_delete_encode_rat l = match l with
  | [] -> (RATIONAL (0,1),[])
  | (f::r) ->
    let x = ExpIntern.decode_exp f
    in
        match x with
           | (RATIONAL (n,d)) -> (x,r)
           | _ -> let (n,l) = find_delete_encode_rat r
                  in (n,f::l) ;;

let rec num_count l = match l with
  | [] -> 0
  | ((NUM n)::r) -> 1+num_count r
  | ((REF x)::r) -> (match ExpIntern.decode_exp (REF x) with | (NUM x) -> 1 | _ -> 0)+num_count r
  | (_::r) -> num_count r
  ;;

let rec add_and_delete p = match p with
  | ([],v) -> ([],v)
  | ((NUM n)::r,v) -> add_and_delete (r,v+n)
  | (f::r,v) ->
    let (r,v) = add_and_delete (r,v)
    in
        (f::r,v)
    ;;

let rec multiply_and_delete p = match p with
  | ([],v) -> ([],v)
  | ((NUM n)::r,v) -> multiply_and_delete (r,v*n)
  | (f::r,v) -> let (r,v) = multiply_and_delete (r,v)
    in
        (f::r,v)
    ;;

let rec rat_count l = match l with
  | [] -> 0
  | ((RATIONAL (n,d))::r) -> 1+rat_count r
  | ((REF x)::r) -> (match ExpIntern.decode_exp (REF x) with (RATIONAL (n,d)) -> 1 | _ -> 0)+rat_count r
  | (_::r) -> rat_count r ;;

let rec add_rat_and_delete p = match p with
  | ([],v) -> ([],v)
  | ((RATIONAL (n,d))::r,v) -> add_rat_and_delete (r,rat_plus v (RATIONAL (n,d)))
  | (f::r,v) ->
    let (r,v) = add_rat_and_delete (r,v)
    in
        (f::r,v)
    ;;

let rec multiply_rat_and_delete p = match p with
  | ([],v) -> ([],v)
  | ((RATIONAL (n,d))::r,v) -> multiply_rat_and_delete (r,rat_times (RATIONAL (n,d)) v)
  | (f::r,v) ->
    let (r,v) = multiply_rat_and_delete (r,v)
    in
        (f::r,v)
    ;;

let rec find_delete_num l = match l with
  | ((NUM x)::r) -> (x,r)
  | (f::r) ->
    let (n,r) = find_delete_num r in (n,f::r) ;;

let rec find_delete_rat p = match p with
  | ((RATIONAL (n,d))::r) -> (RATIONAL (n,d),r)
  | (f::r) ->
    let (n,r) = find_delete_rat r in (n,f::r) ;;

let rec mkBrTerm l = match l with
  | [] -> (NUM 1)
  | [x] -> x
  | l -> (APPL (78,l)) ;;

let rec find_like env n t l = match l with
  | [] -> []
  | (APPL (78,ll)::r) ->
    let (n1,br1) = find_delete_encode_num ll in
    let br = mkBrTerm br1 in
        if not(n1=0) && Match.equal env br t then
            [((APPL (intern_nat_times,(NUM (n+n1))::br1))::r)]
        else
            let l = find_like env n t r in
                if l=[] then []
                else
                    [(APPL (intern_nat_times,ll)::(List.hd l))]
  | (f::r) ->
    let l = find_like env n t r in
        if l=[] then []
        else
            [(f::(List.hd l))]
    ;;

let rec like_terms env l = match l with
  | [] -> []
  | (APPL (78,ll)::r) ->
    let (n,br) = find_delete_encode_num ll in
    let br = mkBrTerm br in
    let fl = if n=0 then [] else find_like env n br r in
        if fl=[] then
            let x = like_terms env r in
            let f = (APPL (intern_nat_times,ll)) in
                if x=[] then []
                else
                    let (APPL (_,l)) = List.hd x in [(APPL (intern_nat_plus,f::l))]
        else
            [(APPL (intern_nat_plus,List.hd fl))]
  | (f::r) ->
    let x = like_terms env r in
        if x=[] then []
        else
            let (APPL (_,l)) = List.hd x in [(APPL (intern_nat_plus,f::l))]
  ;;

let rec mkRatBrTerm l = match l with
  | [] -> (RATIONAL (1,1))
  | [x] -> x
  | l -> (APPL (78,l)) ;;

let rec remove_quant_vars l l2 = match l2 with
  | [] -> []
  | ((q,t)::b) ->
    if List.mem q l then ((q,t)::(remove_quant_vars l b)) else
    remove_quant_vars l b ;;

let rec find_rat_like env n t l = match l with
  | [] -> []
  | (APPL (84,ll)::r) ->
    let (n1,br1) = find_delete_encode_rat ll in
    let br = mkRatBrTerm br1 in
        if not(n1=RATIONAL (0,1)) && Match.equal env br t then
            [((APPL (intern_nat_times,(rat_plus n1 n)::br1))::r)]
        else
            let l = find_rat_like env n t r in
                if l=[] then []
                else
                    [(APPL (intern_rat_times,ll)::(List.hd l))]
  | (f::r) ->
    let l = find_rat_like env n t r in
        if l=[] then []
        else
            [(f::(List.hd l))]
    ;;

let rec rat_like_terms env l = match l with
  | [] -> []
  | (APPL (84,ll)::r) ->
    let (n,br) = find_delete_encode_rat ll in
    let br = mkBrTerm br in
    let fl = if n=RATIONAL (0,1) then [] else find_rat_like env n br r in
        if fl=[] then
            let x = rat_like_terms env r in
            let f = (APPL (intern_rat_times,ll)) in
                if x=[] then []
                else
                    let (APPL (_,l)) = List.hd x in [(APPL (intern_rat_plus,f::l))]
        else
            [(APPL (intern_rat_plus,List.hd fl))]
  | (f::r) ->
    let x = rat_like_terms env r in
        if x=[] then []
        else
            let (APPL (_,l)) = List.hd x in [(APPL (intern_rat_plus,f::l))]
    ;;

let intern_exp_true = ExpIntern.intern_exp (fun (x) -> false) (APPL (intern_true,[])) ;; 

exception NoIf ;;

let rec first_if x l = match l with
  | [] -> raise NoIf
  | ((APPL (f,[c;t;e]))::r) ->
    if f = intern_if then
        (x,(APPL (f,[c;t;e])),r)
    else
         first_if (x@[(APPL (f,[c;t;e]))]) r
  | (f::r) -> first_if (x@[f]) r ;;

let rec is_finite_clause_list env l = match l with
  | ((APPL (f,[]),_)::_) ->
    Env.isFiniteConstructor env f
  | _ -> false ;;

let isFiniteTerm env e = match e with
  | (VAR x) ->
    (try let v = Env.getVarType env x
     in
         Type.isFiniteetype (Env.getTypeDefinition env (Type.getetypeName v))
     with Env.UndefinedSymbol _ -> false)
  | (APPL (f,[])) -> Env.isFiniteConstructor env f
  | (APPL (f,l)) ->
    let t = Type.getReturnetype (Env.getType env f)
    in
        Type.isFiniteetype (Env.getTypeDefinition env (Type.getetypeName t))
  | _ -> false
  ;;

let rec getTermType env e = match e with
  | (VAR x) -> (try Env.getVarType env x with Env.UndefinedSymbol _ -> Type.notype)
  | (APPL (f,l)) -> Type.getReturnetype (Env.getType env f)
  | _ -> Type.notype ;;

let create_equal_case env l r =
    let t = getTermType env l in
    let t = if t=Type.notype then getTermType env r else t in
    let td = Env.getTypeDefinition env (Type.getetypeName t) in
    let tc = Type.getConstructorList td in
        (CASE (l,Type.notype,
              (List.map (fun (x) ->
                       ((APPL (x,[])),CASE (r,Type.notype,
                             (List.map (fun (y) ->
                                  ((APPL (y,[])),APPL ((if x=y then intern_true else intern_false),[]))
                                  ) tc)))
                   ) tc)))
    ;;

let rec find_inversion env e = match e with
  | (CASE (e,_,cl)) ->
    let REF t1 = ExpIntern.intern_exp (Env.isACorC env) e in
    let rec find_smaller_case (f,t1) l = match l with
          | [] -> (f,t1)
          | ((APPL (f1,_),CASE (e1,t,cl))::r) ->
            let REF t2= ExpIntern.intern_exp (Env.isACorC env) e1
            in
                if t2<t1 && is_finite_clause_list env cl then find_smaller_case (f1,t2) r
                else find_smaller_case (f,t1) r
          | (_::r) -> find_smaller_case (f,t1) r in
    let rec find_case l = match l with
          | [] -> (0,0)
          | ((APPL (f,_),CASE (e1,t,cl))::r) ->
            let REF t2= ExpIntern.intern_exp (Env.isACorC env) e1
            in
                if t2<t1 && is_finite_clause_list env cl then find_smaller_case (f,t2) r else find_case r
          | (_::r) -> find_case r in
    let (f,_) = find_case cl in
        f
    ;;

let rec invert_clauses f (CASE (e,t,cl)) =
    let rec find_clause f ((APPL (g,_),ex)::r) =
            if f=g then ex else find_clause f r in
    let (CASE (e1,t1,cl1)) = find_clause f cl
    in
        (CASE (e1,t1,
            (List.map
                (fun (APPL (x,_),term) ->
                    (APPL (x,[]),
                        (CASE (e,t,List.map
                            (fun (APPL (y,_),term2) ->
                                 if f=y then (APPL (y,[]),term)
                                        else (APPL (y,[]),term2)
                            ) cl))
                    )
                ) cl1
            )
        ))
    ;;

(*fun reduce_case x t ((VAR y,l)::_) =
    [Subst.subst (Subst.addPair empty y x) l]
  | reduce_case (APPL (f,l)) t cl =
    if is_constructor f then
        let fun relevant_cases [] = []
              | relevant_cases ((APPL (g,l2),t)::r) =
                if f=g then
                    ((APPL (g,l2),t)::relevant_cases r)
                else
                    relevant_cases r
              | relevant_cases ((VAR x,t)::r) = (VAR x,t)::(relevant_cases r)
              | relevant_cases (_::r) = relevant_cases r
            val relevant_cases = relevant_cases cl
            fun build_case l [] rc =
                if List.length rc=0 then
                    [(APPL (intern_undef,[]))]
                else
                    let val (_,t) = List.hd rc in [t] end
              | built_case l (f::r) rc =
                let
                in
                    (CASE (f,Type.notype,))
                end
        in
            build_case l relevant_cases
        end
    else []
  | reduce_case _ _ _ = []*)

let rec reduce_finite_case ee t cases = match ee with
  | (APPL (f,[])) ->
    let rec find_case l = match l with
          | [] -> []
          | ((c,e)::r) ->
            if c=(APPL (f,[])) then [e] else find_case r
    in
    find_case cases
    | _ -> []
  ;;

let rec builtin rewrite env e = match e with
  | (APPL (76,[])) -> [NUM 0]
  | (APPL (82,[])) -> [RATIONAL(0,1)]
  | (APPL (78,[])) -> [NUM 1]
  | (APPL (84,[])) -> [RATIONAL (1,1)]
  | (APPL (80,[NUM x;NUM y])) ->
    if x < y then
        [APPL (intern_true,[])]
    else
        [APPL (intern_false,[])]
  | (APPL (86,[RATIONAL (xn,xd);RATIONAL (yn,yd)])) ->
    if xn*yd < yn*xd then
        [APPL (intern_true,[])]
    else
        [APPL (intern_false,[])]
  | (APPL (76,[x])) -> [x]
  | (APPL (78,[x])) -> [x]
  | (APPL (82,[x])) -> [x]
  | (APPL (84,[x])) -> [x]
  | (APPL (76,l)) ->
    if num_count l>1 then
        let (r,v) = add_and_delete (l,0)
        in
            [APPL (76,Mylist.delete (NUM 0) ((NUM v)::r))]
    else if List.mem (NUM 0) l then
        [APPL (76,Mylist.delete (NUM 0) l)]
    else like_terms env l
  | (APPL (82,l)) ->
    if rat_count l>1 then
        let (r,v) = add_rat_and_delete (l,RATIONAL (0,1))
        in
            [APPL (82,Mylist.delete (RATIONAL (0,1)) (v::r))]
    else if List.mem (RATIONAL (0,1)) l then
        [APPL (82,Mylist.delete (RATIONAL (0,1)) l)]
    else rat_like_terms env l
  | (APPL (78,l)) ->
    if List.mem (NUM 0) l then
        [NUM 0]
    else if num_count l>1 then
        let (r,v) = multiply_and_delete (l,1)
        in
            [APPL (78,Mylist.delete (NUM 1) ((NUM v)::r))]
    else if List.mem (RATIONAL (1,1)) l then
        [APPL (78,Mylist.delete (RATIONAL (1,1)) l)]
    else
        []
  | (APPL (84,l)) ->
    if List.mem (NUM 0) l then
        [NUM 0]
    else if rat_count l>1 then
        let (r,v) = multiply_rat_and_delete (l,RATIONAL (1,1))
        in
            [APPL (84,Mylist.delete (RATIONAL (1,1)) (v::r))]
    else if List.mem (RATIONAL (1,1)) l then
        [APPL (84,Mylist.delete (RATIONAL (1,1)) l)]
    else
        []
  | (APPL (85,[RATIONAL (n1,d1);RATIONAL (n2,d2)])) ->
    if not(n2=0) then [rat_divide (RATIONAL (n1,d1)) (RATIONAL (n2,d2))] else []
  | (APPL (79,[NUM a;NUM b])) ->
    if not(b=0) then [NUM (a / b)] else []
  | (APPL (81,[NUM a;NUM b])) ->
    if not(b=0) then [NUM (a mod b)] else []
  | (APPL (77,[NUM a;NUM b])) ->
    [NUM (a-b)]
  | (APPL (83,[RATIONAL (n1,d1);RATIONAL (n2,d2)])) ->
    [rat_minus (RATIONAL (n1,d1)) (RATIONAL (n2,d2))]
  | (APPL (77,[APPL (76,l);NUM b])) ->
    if num_count l=1 then
        let (l,n) = add_and_delete (l,0)
        in
            if n > b then
                [(APPL (76,(NUM (n-b))::l))]
            else
                [(APPL (77,[APPL (76,l);(NUM (b-n))]))]
    else
        []
  | (APPL (83,[APPL (82,l);RATIONAL (n2,d2)])) ->
    if rat_count l=1 then
        let (l,RATIONAL (n1,d1)) = add_rat_and_delete (l,RATIONAL (0,1))
        in
            if n1*d2 > n2*d1 then
                [(APPL (82,(rat_minus (RATIONAL (n1,d1)) (RATIONAL (n2,d2)))::l))]
            else
                [(APPL (83,[APPL (82,l);(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))]))]
    else
        []
  | (APPL (77,[APPL (76,l1);APPL (76,l2)])) ->
    if num_count l1=1 && num_count l2=1 then
        let (l1,n1) = add_and_delete (l1,0) in
        let (l2,n2) = add_and_delete (l2,0) in
            if n1 > n2 then
                [(APPL (77,[APPL (76,(NUM (n1-n2))::l1);APPL (76,l2)]))]
            else
                [(APPL (77,[APPL (76,l1);APPL (76,(NUM (n2-n1))::l2)]))]
    else
        []
  | (APPL (83,[APPL (82,l1);APPL (82,l2)])) ->
    if rat_count l1=1 && num_count l2=1 then
        let (l1,RATIONAL (n1,d1)) = add_rat_and_delete (l1,RATIONAL (0,1)) in
        let (l2,RATIONAL (n2,d2)) = add_rat_and_delete (l2,RATIONAL (0,1)) in
            if n1*d2 > n2*d1 then
                [(APPL (83,[APPL (82,(rat_minus (RATIONAL (n1,d1)) (RATIONAL (n2,d2)))::l1);APPL (82,l2)]))]
            else
                [(APPL (83,[APPL (82,l1);APPL (82,(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)]))]
    else
        []
  | (APPL (77,[NUM b;APPL (76,l)])) ->
    if num_count l=1 then
        let (l,n) = add_and_delete (l,0)
        in
            if b > n then
                [(APPL (77,[(NUM (b-n));(APPL (76,l))]))]
            else
                []
    else
        []
  | (APPL (83,[RATIONAL (n2,d2);APPL (82,l)])) ->
    if rat_count l=1 then
        let (l,RATIONAL (n1,d1)) = add_rat_and_delete (l,RATIONAL (0,1))
        in
            if n2*d1 > n1*d2 then
                [(APPL (83,[(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)));(APPL (82,l))]))]
            else
                []
    else
        []
  | (APPL (66,[STRING s])) -> [NUM (String.length s)]
  | (APPL (67,[STRING s;NUM n])) ->
    if n < String.length s then [CHAR (String.get s n)] else []
  | (APPL (68,[CHAR c;STRING s])) -> [STRING ((String.make 1 c) ^ s)]
  | (APPL (69,[NUM n])) -> [CHAR (char_of_int n)]
  | (APPL (69,[CHAR c])) -> [NUM (int_of_char c)]
  | (APPL (9,[x])) -> [NORMAL x]
  | (APPL (9,[])) -> [APPL (intern_true,[])]
  | (APPL (9,l)) -> if List.mem (APPL (intern_false,[])) l then
        [APPL (intern_false,[])]
    else let l2 = and_remove l in
        if l = l2 then
            []
        else
            [APPL (intern_and,List.map (fun (x) -> (NORMAL x)) l2)]
  | (APPL (10,[])) -> [APPL (intern_false,[])]
  | (APPL (10,[x])) -> [NORMAL x]
  | (APPL (10,l)) ->
    if List.mem (APPL (intern_true,[])) l then
        [APPL (intern_true,[])]
    else let l2 = or_remove l in
        if l = l2 then
            []
        else
            [(APPL (intern_or,List.map (fun (x) -> (NORMAL x)) l2))]
  | (APPL (17,[APPL (17,[x])])) -> [NORMAL x]
  | (APPL (17,[APPL (4,[])])) -> [APPL (intern_false,[])]
  | (APPL (17,[APPL (5,[])])) -> [APPL (intern_true,[])]
  | (APPL (17,[APPL (9,l)])) ->
      [APPL (intern_or,List.map (fun (x) -> (APPL (intern_not,[NORMAL x]))) l)]
  | (APPL (17,[APPL (10,l)])) ->
      [APPL (intern_and,List.map (fun (x) -> (APPL (intern_not,[NORMAL x]))) l)]
  | (APPL (72,[x])) ->
    let c = ExpIntern.intern_exp (Env.isACorC env) x in
    let cl= List.filter
            (fun (y) -> Crewrite.relevant_rule env c (REF y))
            (Env.getContextList env) in
    let _ = Trace.trace "rewriteRule" (fun (x) -> "Building relevant rule list") in
    let _ = Trace.trace "rewriteRule" (fun (z) -> ("exp " ^ (prExp (ExpIntern.decode_exp x)) ^ "\n")) in
    let _ = Trace.trace_list "rewriteRule" (fun (x) -> (List.map (fun (x) -> "cl " ^ (prExp (ExpIntern.decode_exp (REF x))) ^ "\n") cl))
    in
        if cl=[] then
            []
        else
        [APPL (intern_default,x::(List.map (fun (x) -> REF x) cl))]
  | (APPL (71,x::l)) ->
    let env1 = Env.addContextRules (Env.clearContextRules env) l in
    let rl= rewrite env1 x
    in
        if List.mem (APPL (intern_true,[])) rl ||
           List.mem intern_exp_true rl then
            []
        else
            [APPL (intern_true,[])]
  | (APPL (11,[APPL (76,l1);APPL (76,l2)])) ->
    if num_count l1 = 1 && num_count l2 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
    let (n2,l2) = find_delete_encode_num l2 in
        if n1>n2 then
            [(APPL (11,[APPL (76,(NUM (n1-n2))::l1);APPL (76,l2)]))]
        else
            [(APPL (11,[APPL (76,l1);APPL (76,(NUM (n2-n1))::l2)]))]
    else if Match.equal env (APPL (76,l1)) (APPL (76,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (11,[APPL (82,l1);APPL (82,l2)])) ->
    if rat_count l1 = 1 && rat_count l2 = 1 then
    let (RATIONAL (n1,d1),l1) = find_delete_encode_rat l1 in
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if n1*d2>n2*d1 then
            [(APPL (11,[APPL (76,(rat_minus (RATIONAL (n1,d1)) (RATIONAL (n2,d2)))::l1);APPL (76,l2)]))]
        else
            [(APPL (11,[APPL (76,l1);APPL (76,(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)]))]
    else if Match.equal env (APPL (82,l1)) (APPL (82,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (11,[APPL (78,l1);APPL (78,l2)])) ->
    if num_count l1 = 1 && num_count l2 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
    let (n2,l2) = find_delete_encode_num l2 in
    let g = gcd n1 n2 in
        if g=1 || g=0 then
            []
        else if g=n2 then
            [(APPL (11,[APPL (78,(NUM (n1 / n2))::l1);APPL (78,l2)]))]
        else if not(n1=0) && n2 mod n1=0 then
            [(APPL (11,[APPL (78,l1);APPL (78,(NUM (n2 / n1))::l2)]))]
        else
            [(APPL (11,[APPL (78,(NUM (n1 / g)::l1));APPL (78,(NUM (n2 / g))::l2)]))]
    else if Match.equal env (APPL (78,l1)) (APPL (78,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (11,[APPL (84,l1);APPL (84,l2)])) ->
    if rat_count l1 = 1 && rat_count l2 = 1 then
    let (RATIONAL (n1,d1),l1) = find_delete_encode_rat l1 in
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if n2=0 then
            []
        else
            [(APPL (11,[APPL (84,(rat_divide (RATIONAL (n1,d1)) (RATIONAL (n2,d2)))::l1);APPL (84,l2)]))]
    else if Match.equal env (APPL (84,l1)) (APPL (84,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (11,[NUM n1;APPL (76,l2)])) ->
    if num_count l2 = 1 then
    let (n2,l2) = find_delete_encode_num l2
    in
        if not(n1=0) then
           (if n1>n2 then
                [(APPL (11,[NUM (n1-n2);APPL (76,l2)]))]
            else
                [(APPL (11,[NUM 0;APPL (76,(NUM (n2-n1))::l2)]))])
        else []
    else []
  | (APPL (11,[RATIONAL (n1,d1);APPL (82,l2)])) ->
    if rat_count l2 = 1 then
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if not(n1=0) then
            [(APPL (11,[RATIONAL (0,1);APPL (76,(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)]))]
                else []
    else []
  | (APPL (11,[NUM n1;APPL (78,l2)])) ->
    if num_count l2 = 1 then
    let (n2,l2) = find_delete_encode_num l2 in
    let g = gcd n1 n2 in
        if n1=0 then
            [(APPL (11,[NUM 0;APPL (78,l2)]))]
        else if g=1 || g=0 then
            []
        else if g=n2 then
            [(APPL (11,[NUM (n1 / n2);APPL (78,l2)]))]
        else if g=n1 then
            [(APPL (11,[NUM 1;APPL (78,NUM (n2 / n1)::l2)]))]
        else
            [(APPL (11,[NUM (n1 / g);APPL (78,NUM (n2 / g)::l2)]))]
    else []
  | (APPL (11,[RATIONAL (n1,d1);APPL (84,l2)])) ->
    if rat_count l2 = 1 then
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2
    in
        if n1=0 then
            [(APPL (11,[RATIONAL (0,1);APPL (78,l2)]))]
        else if n2=0 then
            []
        else
            [(APPL (11,[rat_divide (RATIONAL (n1,d1)) (RATIONAL (n2,d2));APPL (78,l2)]))]
    else []
  | (APPL (11,[APPL (76,l1);(NUM n2)])) ->
    if num_count l1 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
        if not(n2=0) then
           (if n1>n2 then
                [(APPL (11,[APPL (76,(NUM (n1-n2))::l1);(NUM 0)]))]
            else
                [(APPL (11,[APPL (76,l1);NUM (n2-n1)]))])
        else []
    else []
  | (APPL (11,[APPL (82,l1);RATIONAL (n2,d2)])) ->
    if rat_count l1 = 1 then
    let (RATIONAL (n1,d1),l1) = find_delete_encode_rat l1 in
        if not(n2=0) then
           [(APPL (11,[RATIONAL (0,1);APPL (76,(rat_minus (RATIONAL (n1,d1)) (RATIONAL (n2,d2)))::l1)]))]
        else []
    else []
  | (APPL (11,[APPL (78,l2);NUM n1])) ->
    if num_count l2 = 1 then
    let (n2,l2) = find_delete_encode_num l2 in
    let g = gcd n1 n2 in
        if n1=0 then
            [(APPL (11,[APPL (78,l2);NUM 0]))]
        else if g=1 || g=0 then
            []
        else if g=n2 then
            [(APPL (11,[APPL (78,l2);NUM (n1 / n2)]))]
        else if g=n1 then
            [(APPL (11,[APPL (78,NUM (n2 / n1)::l2);NUM 1]))]
        else
            [(APPL (11,[APPL (78,NUM (n2 / g)::l2);NUM (n1 / g)]))]
    else []
  | (APPL (11,[APPL (84,l2);RATIONAL (n1,d1)])) ->
    if rat_count l2 = 1 then
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if n1=0 then
            [(APPL (11,[RATIONAL (0,1);APPL (78,l2)]))]
        else if n2=0 then
            []
        else
            [(APPL (11,[rat_divide (RATIONAL (n1,d1)) (RATIONAL (n2,d2));APPL (78,l2)]))]
    else []
  | (APPL (11,[APPL (78,l1);APPL (76,l2)])) ->
    if member_eq env (APPL (78,l1)) l2 then
    let l2 = delete_eq env (APPL (78,l1)) l2
    in
        [(APPL (11,[NUM 0;APPL (76,l2)]))]
    else if member_eq env (APPL (76,l2)) l1 then
    let l1 = delete_eq env (APPL (76,l2)) l1
    in
        [(APPL (11,[APPL (78,l2);NUM 1]))]
    else []
  | (APPL (11,[APPL (76,l1);APPL (78,l2)])) ->
    if member_eq env (APPL (76,l1)) l2 then
    let l2 = delete_eq env (APPL (76,l1)) l2
    in
        [(APPL (11,[NUM 1;APPL (78,l2)]))]
    else if member_eq env (APPL (78,l2)) l1 then
    let l1 = delete_eq env (APPL (78,l2)) l1
    in
        [(APPL (11,[APPL (76,l2);NUM 0]))]
    else []
  | (APPL (11,[x;APPL (76,l2)])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2
    in
        [(APPL (11,[NUM 0;APPL (76,l2)]))]
    else if Match.equal env x (APPL (76,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (11,[APPL (76,l2);x])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2
    in
        [(APPL (11,[APPL (76,l2);NUM 0]))]
    else if Match.equal env x (APPL (76,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (11,[x;APPL (78,l2)])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2 in
        [(APPL (11,[NUM 1;APPL (78,l2)]))]
    else if Match.equal env x (APPL (78,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (11,[APPL (78,l2);x])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2
    in
        [(APPL (11,[APPL (78,l2);NUM 1]))]
    else if Match.equal env x (APPL (78,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (80,[APPL (76,l1);APPL (76,l2)])) ->
    if num_count l1 = 1 && num_count l2 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
    let (n2,l2) = find_delete_encode_num l2 in
        if n1>n2 then
            [(APPL (80,[APPL (76,(NUM (n1-n2))::l1);APPL (76,l2)]))]
        else
            [(APPL (80,[APPL (76,l1);APPL (76,(NUM (n2-n1))::l2)]))]
    else []
  | (APPL (86,[APPL (82,l1);APPL (82,l2)])) ->
    if rat_count l1 = 1 && rat_count l2 = 1 then
    let (RATIONAL (n1,d1),l1) = find_delete_encode_rat l1 in
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if n1*d2>n2*d1 then
            [(APPL (86,[APPL (82,(rat_minus (RATIONAL (n1,d1)) (RATIONAL (n2,d2)))::l1);APPL (82,l2)]))]
        else
            [(APPL (86,[APPL (82,l1);APPL (82,(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)]))]
    else []
  | (APPL (80,[APPL (78,l1);APPL (78,l2)])) ->
    if num_count l1 = 1 && num_count l2 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
    let (n2,l2) = find_delete_encode_num l2 in
    let g = gcd n1 n2 in
        if g=1 || g=0 then
            []
        else if g=n2 then
            (if g < 0 then
                 [(APPL (80,[APPL (78,l2);APPL (78,(NUM (n1 / n2))::l1)]))]
             else
                 [(APPL (80,[APPL (78,(NUM (n1 / n2))::l1);APPL (78,l2)]))])
        else if not(n1=0) && n2 mod n1=0 then
            (if n1 < 0 then
                 [(APPL (80,[APPL (78,(NUM (n2 / n1))::l2);APPL (78,l1)]))]
             else
                 [(APPL (80,[APPL (78,l1);APPL (78,(NUM (n2 / n1))::l2)]))])
        else
            (if g < 0 then
                 [(APPL (80,[APPL (78,(NUM (n2 / g))::l2);APPL (78,(NUM (n1 / g)::l1));]))]
             else
                 [(APPL (80,[APPL (78,(NUM (n1 / g)::l1));APPL (78,(NUM (n2 / g))::l2)]))])
    else if Match.equal env (APPL (78,l1)) (APPL (78,l2)) then
        [(APPL (intern_false,[]))]
    else []
  | (APPL (86,[APPL (84,l1);APPL (84,l2)])) ->
    if rat_count l1 = 1 && rat_count l2 = 1 then
    let (RATIONAL (n1,d1),l1) = find_delete_encode_rat l1 in
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if n1*d2=n2*d1 then
            (if (n1 < 0 && d1 > 0) || (n1 > 0 && d1 < 0) then
                 [(APPL (86,[APPL (84,l2);APPL (84,l1)]))]
             else
                 [(APPL (86,[APPL (84,l1);APPL (84,l2)]))])
        else if n1=0 then
            []
        else
            (if (n1 < 0 && d1 > 0) || (n1 > 0 && d1 < 0) then
                 [(APPL (86,[APPL (84,(rat_divide (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2);APPL (84,l1)]))]
             else
                 [(APPL (86,[APPL (84,l1);APPL (84,(rat_divide (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)]))])
    else []
  | (APPL (80,[NUM n1;APPL (76,l2)])) ->
    if num_count l2 = 1 then
    let (n2,l2) = find_delete_encode_num l2 in
        if not(n1=0) then
           (if n1>n2 then
                [(APPL (80,[NUM (n1-n2);APPL (76,l2)]))]
            else
                [(APPL (80,[NUM 0;APPL (76,(NUM (n2-n1))::l2)]))])
        else []
    else []
  | (APPL (86,[RATIONAL (n1,d1);APPL (82,l2)])) ->
    if rat_count l2 = 1 then
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if not(n1=0) then
            [(APPL (80,[RATIONAL (0,1);APPL (76,(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)]))]
        else []
    else []
  | (APPL (80,[NUM n1;APPL (78,l2)])) ->
    if num_count l2 = 1 then
    let (n2,l2) = find_delete_encode_num l2 in
    let g = gcd n1 n2 in
        if n1=0 then
            [(APPL (80,[NUM 0;APPL (78,l2)]))]
        else if g=1 || g=0 then
            []
        else if g=n2 then
            (if n2 < 0 then
                [(APPL (80,[APPL (78,l2);NUM (n1 / n2)]))]
            else
                [(APPL (80,[NUM (n1 / n2);APPL (78,l2)]))])
        else if g=n1 then
            (if n1 < 0 then
                 [(APPL (80,[APPL (78,NUM (n2 / n1)::l2);NUM 1]))]
             else
                 [(APPL (80,[NUM 1;APPL (78,NUM (n2 / n1)::l2)]))])
        else
            (if g < 0 then
                 [(APPL (80,[APPL (78,NUM (n2 / g)::l2);NUM (n1 / g)]))]
             else
                 [(APPL (80,[NUM (n1 / g);APPL (78,NUM (n2 / g)::l2)]))])
    else []
  | (APPL (86,[RATIONAL (n1,d1);APPL (84,l2)])) ->
    if rat_count l2 = 1 then
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2
    in
        if not(n2=0) then
            (if n1=0 then
                 (if n2>0 then
                      [(APPL (86,[RATIONAL (0,1);APPL (84,l2)]))]
                  else
                      [(APPL (86,[APPL (84,l2)]));RATIONAL (0,1)])
             else
                 (if n1=1 && d1=1 then
                     []
                  else if n1>0 then
                     [(APPL (86,[RATIONAL (1,1);APPL (84,(rat_divide (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)]))]
                  else
                     [(APPL (86,[APPL (84,(rat_divide (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2);RATIONAL (1,1)]))]))
        else []
    else []
  | (APPL (80,[APPL (76,l1);(NUM n2)])) ->
    if num_count l1 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
        if not(n2=0) then
           (if n1>n2 then
                [(APPL (80,[APPL (76,(NUM (n1-n2))::l1);(NUM 0)]))]
            else
                [(APPL (80,[APPL (76,l1);NUM (n2-n1)]))])
        else []
    else []
  | (APPL (86,[APPL (82,l2);RATIONAL (n1,d1)])) ->
    if rat_count l2 = 1 then
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2
    in
        if not(n1=0) then
            [(APPL (80,[APPL (76,(rat_minus (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2);RATIONAL (0,1)]))]
        else []
    else []
  | (APPL (80,[APPL (78,l2);NUM n1])) ->
    if num_count l2 = 1 then
    let (n2,l2) = find_delete_encode_num l2 in
    let g = gcd n1 n2 in
        if n1=0 then
            [(APPL (80,[APPL (78,l2);NUM 0]))]
        else if g=1 || g=0 then
            []
        else if g=n2 then
            (if n2 < 0 then
                 [(APPL (80,[NUM (n1 / n2);APPL (78,l2)]))]
             else
                 [(APPL (80,[APPL (78,l2);NUM (n1 / n2)]))])
        else if g=n1 then
            (if n1 < 0 then
                 [(APPL (80,[NUM 1;APPL (78,NUM (n2 / n1)::l2)]))]
             else
                 [(APPL (80,[APPL (78,NUM (n2 / n1)::l2);NUM 1]))])
        else
            (if g < 0 then
                 [(APPL (80,[NUM (n1 / g);APPL (78,NUM (n2 / g)::l2)]))]
             else
                 [(APPL (80,[APPL (78,NUM (n2 / g)::l2);NUM (n1 / g)]))])
    else []
  | (APPL (86,[APPL (84,l2);RATIONAL (n1,d1)])) ->
    if rat_count l2 = 1 then
    let (RATIONAL (n2,d2),l2) = find_delete_encode_rat l2 in
        if not(n2=0) then
            (if n1=0 then
                 (if n2<0 then
                      [(APPL (86,[RATIONAL (0,1);APPL (84,l2)]))]
                  else
                      [(APPL (86,[APPL (84,l2)]));RATIONAL (0,1)])
             else
                 (if n1=1 && d1=1 then
                     []
                  else if n1<0 then
                     [APPL (86,[RATIONAL (1,1);APPL (84,(rat_divide (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2)])]
                  else
                     [APPL (86,[APPL (84,(rat_divide (RATIONAL (n2,d2)) (RATIONAL (n1,d1)))::l2);RATIONAL (1,1)])]))
        else []
    else []
  | (APPL (80,[x;APPL (76,l2)])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2 in
        [(APPL (80,[NUM 0;APPL (76,l2)]))]
    else []
  | (APPL (86,[x;APPL (82,l2)])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2 in
        [(APPL (86,[RATIONAL (0,1);APPL (82,l2)]))]
    else []
  (*| builtin _ env (APPL (80,[x,APPL (78,l2)])) =
    if member_eq env x l2 then
    let val l2 = delete_eq env x l2
    in
        [(APPL (80,[NUM 1,APPL (76,l2)]))]
    end else []*)
  | (APPL (80,[APPL (76,l2);x])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2 in
        [(APPL (80,[APPL (76,l2);NUM 0]))]
    else []
  | (APPL (86,[APPL (82,l2);x])) ->
    if member_eq env x l2 then
    let l2 = delete_eq env x l2 in
        [(APPL (86,[APPL (82,l2);RATIONAL (0,1)]))]
    else []
  (*| builtin _ env (APPL (80,[APPL (78,l2),x])) =
    if member_eq env x l2 then
    let val l2 = delete_eq env x l2
    in
        [(APPL (80,[APPL (78,l2),NUM 1]))]
            end else []*)
  (*| builtin _ env (APPL (12,[APPL (76,l1),APPL (76,l2)])) =
    if num_count l1 = 1 && num_count l2 = 1 then
    let val (n1,l1) = find_delete_encode_num l1
        val (n2,l2) = find_delete_encode_num l2
    in
        if n1>n2 then
            [(APPL (12,[APPL (76,(NUM (n1-n2))::l1),APPL (76,l2)]))]
        else
            [(APPL (12,[APPL (76,l1),APPL (76,(NUM (n2-n1))::l2)]))]
    end else []
  | builtin _ env (APPL (12,[APPL (78,l1),APPL (78,l2)])) =
    if num_count l1 = 1 && num_count l2 = 1 then
    let val (n1,l1) = find_delete_encode_num l1
        val (n2,l2) = find_delete_encode_num l2
    in
        if not(n2=0) && n1 mod n2=0 then
            [(APPL (12,[APPL (78,(NUM (n1 / n2))::l1),APPL (78,l2)]))]
        else if not(n1=0) && n2 mod n1=0 then
            [(APPL (12,[APPL (78,l1),APPL (78,(NUM (n2 / n1))::l2)]))]
        else []
    end else []
  | builtin _ env (APPL (12,[NUM n1,APPL (76,l2)])) =
    if num_count l2 = 1 then
    let val (n2,l2) = find_delete_encode_num l2
    in
        if not(n1=0) then
           (if n1>n2 then
                [(APPL (12,[NUM (n1-n2),APPL (76,l2)]))]
            else
                [(APPL (12,[NUM 0,APPL (76,(NUM (n2-n1))::l2)]))])
        else []
    end else []
  | builtin _ env (APPL (12,[NUM n1,APPL (78,l2)])) =
    if num_count l2 = 1 then
    let val (n2,l2) = find_delete_encode_num l2
    in
        if not(n2=0) && (n1 mod n2)=0 then
            [(APPL (12,[NUM (n1 / n2),APPL (78,l2)]))]
        else if not(n1=0) && (n2 mod n1)=0 then
            [(APPL (12,[NUM 1,APPL (78,NUM (n2 / n1)::l2)]))]
        else []
    end else []
  | builtin _ env (APPL (12,[APPL (76,l1),(NUM n2)])) =
    if num_count l1 = 1 then
    let val (n1,l1) = find_delete_encode_num l1
    in
        if not(n2=0) then
           (if n1>n2 then
                [(APPL (12,[APPL (76,(NUM (n1-n2))::l1),(NUM 0)]))]
            else
                [(APPL (12,[APPL (76,l1),NUM (n2-n1)]))])
        else []
    end else []
  | builtin _ env (APPL (12,[APPL (78,l2),NUM n1])) =
    if num_count l2 = 1 then
    let val (n2,l2) = find_delete_encode_num l2
    in
        if not(n2=0) && (n1 mod n2)=0 then
            [(APPL (12,[APPL (78,l2),NUM (n1 / n2)]))]
        else if not(n1=0) && (n2 mod n1)=0 then
            [(APPL (12,[APPL (78,NUM (n2 / n1)::l2),NUM 1]))]
        else []
    end else []
  | builtin _ env (APPL (12,[x,APPL (76,l2)])) =
    if member_eq env x l2 then
    let val l2 = delete_eq env x l2
    in
        [(APPL (12,[NUM 0,APPL (76,l2)]))]
    end else []
  | builtin _ env (APPL (12,[x,APPL (78,l2)])) =
    if member_eq env x l2 then
    let val l2 = delete_eq env x l2
    in
        [(APPL (12,[NUM 1,APPL (78,l2)]))]
    end else []
  | builtin _ env (APPL (12,[APPL (76,l2),x])) =
    if member_eq env x l2 then
    let val l2 = delete_eq env x l2
    in
        [(APPL (12,[APPL (76,l2),NUM 0]))]
    end else []
  | builtin _ env (APPL (12,[APPL (78,l2),x])) =
    if member_eq env x l2 then
    let val l2 = delete_eq env x l2
    in
        [(APPL (12,[APPL (78,l2),NUM 1]))]
    end else []*)
  | (APPL (11,[APPL (78,l1);APPL (78,l2)])) ->
    if num_count l1 = 1 && num_count l2 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
    let (n2,l2) = find_delete_encode_num l2 in
    let d = gcd n1 n2 in
        if d > 1 then
            [(APPL (11,[APPL (78,(NUM (n1 / d))::l1);APPL (78,(NUM (n2 / d))::l2)]))]
        else []
    else if Match.equal env (APPL (78,l1)) (APPL (78,l2)) then
        [(APPL (intern_true,[]))]
    else []
  | (APPL (80,[APPL (78,l1);APPL (78,l2)])) ->
    if num_count l1 = 1 && num_count l2 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
    let (n2,l2) = find_delete_encode_num l2 in
    let d = gcd n1 n2 in
        if d > 1 then
            [(APPL (80,[APPL (78,(NUM (n1 / d))::l1);APPL (78,(NUM (n2 / d))::l2)]))]
        else []
    else []
  | (APPL (12,[APPL (78,l1);APPL (78,l2)])) ->
    if num_count l1 = 1 && num_count l2 = 1 then
    let (n1,l1) = find_delete_encode_num l1 in
    let (n2,l2) = find_delete_encode_num l2 in
    let d = gcd n1 n2 in
        if d > 1 then
            [(APPL (12,[APPL (78,(NUM (n1 / d))::l1);APPL (78,(NUM (n2 / d))::l2)]))]
        else []
    else []
  | (APPL (11,[(APPL (c1,l1));(APPL (c2,l2))])) ->
    if is_constructor c1 && is_constructor c2 then
        (if c1 = c2 && List.length l1 = List.length l2 then
             [APPL (intern_and,(pairJoin l1 l2))]
         else
             [APPL (intern_false,[])]
        )
    else if (Match.equal env (APPL (c1,l1)) (APPL (c2,l2))) then
        [APPL (intern_true,[])]
    else if isFiniteTerm env (APPL (c1,l1)) || isFiniteTerm env (APPL (c2,l2)) then
        [create_equal_case env (APPL (c1,l1)) (APPL (c2,l2))]
    else
        []
  | (APPL (11,[x;(APPL (s,l))])) ->
      if is_constructor s then
          let l2 = flatten_constructors l in
              if e_member env x l2 then
                  [APPL (intern_false,[])]
              else if isFiniteTerm env x || isFiniteTerm env (APPL (s,l)) then
                  [create_equal_case env x (APPL (s,l))]
              else
                  []
      else if isFiniteTerm env x || isFiniteTerm env (APPL (s,l)) then
          [create_equal_case env x (APPL (s,l))]
      else
          []
  | (APPL (11,[(APPL (s,l));x])) ->
      if is_constructor s then
          let l2 = flatten_constructors l in
              if e_member env x l2 then
                  [APPL (intern_false,[])]
              else if isFiniteTerm env x || isFiniteTerm env (APPL (s,l)) then
                  [create_equal_case env x (APPL (s,l))]
              else
                  []
      else if isFiniteTerm env x || isFiniteTerm env (APPL (s,l)) then
          [create_equal_case env x (APPL (s,l))]
      else
          []
  | (APPL (11,[NUM n1;NUM n2])) ->
    if n1=n2 then
        [APPL (intern_true,[])]
    else
        [APPL (intern_false,[])]
  | (APPL (11,[STRING n1;STRING n2])) ->
    if n1=n2 then
        [APPL (intern_true,[])]
    else
        [APPL (intern_false,[])]
  | (APPL (11,[CHAR n1;CHAR n2])) ->
    if n1=n2 then
        [APPL (intern_true,[])]
    else
        [APPL (intern_false,[])]
  | (APPL (11,[l;r])) ->
      if (Match.equal env l r) then
          [APPL (intern_true,[])]
      else if isFiniteTerm env l || isFiniteTerm env r then
          [create_equal_case env l r]
      else
          []
  | (APPL (12,[(APPL (s1,l1));(APPL (s2,l2))])) ->
    if (is_constructor s1) && (is_constructor s2) then
        (if s1=s2 && (List.length l1)=(List.length l2) then
            [APPL (intern_and,(precPairJoin l1 l2))]
        else if Env.hasSmallerPrecedence env s1 s2 then
            [APPL (intern_and,lpFactor l1 (APPL (s2,l2)))]
        else
            [APPL (intern_or,spFactor (APPL (s1,l1)) l2)]
    ) else if (Match.equal env (APPL (s1,l1)) (APPL (s2,l2))) then
        [APPL (intern_true,[])]
    else if subterm_member env (APPL (s1,l1)) (APPL (s2,l2)) then
        [APPL (intern_true,[])]
    else if (is_constructor s1) && (Env.smallestSymbolOfType env s1)
            && (l1=[]) then
        [APPL (intern_true,[])]
    else
        []
  | (APPL (12,[(CHAR n1);(CHAR n2)])) ->
    if n2<n1 then
        [APPL (intern_false,[])]
    else
        [APPL (intern_true,[])]
  | (APPL (12,[(NUM n1);(NUM n2)])) ->
    if n2<n1 then
        [APPL (intern_false,[])]
    else
        [APPL (intern_true,[])]
  | (APPL (12,[(STRING s1);(STRING s2)])) ->
    if String.length s2 < String.length s1 || (String.length s1=String.length s2 && s2<s1) then
        [APPL (intern_false,[])]
    else
        [APPL (intern_true,[])]
  | (APPL (12,[(MARKED_VAR x);e])) ->
    if (Context.getMarkedVars e)=[] then
        (if (List.mem x (Context.getMarkedVars e)) then
            [APPL (intern_true,[])]
        else
            [])
    else
        [APPL (intern_true,[])]
  | (APPL (12,[(APPL (s,l));(VAR x)])) ->
    if (is_constructor s) &&
       (List.mem x (Context.getFreeVars (APPL (s,l)))) then
        [APPL (intern_false,[])]
    else if (is_constructor s) && (Env.smallestSymbolOfType env s)
            && (l=[]) then
        [APPL (intern_true,[])]
    else
        []
  | (APPL (12,[(APPL (s,l));(MARKED_VAR x)])) ->
    [APPL (intern_false,[])]
  | (APPL (12,[(APPL (s,l));b])) ->
    if (subterm_member env (APPL (s,l)) b) then
        [APPL (intern_true,[])]
    else if (is_constructor s) && (Env.smallestSymbolOfType env s)
       && (l=[]) then
        [APPL (intern_true,[])]
    else
        [APPL (intern_false,[])]
  | (APPL (12,[a;b])) ->
    if (subterm_member env a b) then
        [APPL (intern_true,[])]
    else
        []
  | (APPL (18,[APPL (4,[]);p;e])) -> [NORMAL p]
  | (APPL (18,[APPL (5,[]);p;e])) -> [NORMAL e]
  | (APPL (18,[APPL (18,[c;p;e]);p2;e2])) ->
      [APPL (18,[NORMAL c;APPL (18,[NORMAL p;NORMAL p2;NORMAL e2]);APPL(18,[NORMAL e;NORMAL p2;NORMAL e2])])]
  | (APPL (18,[c;t;f])) ->
    if (returns_bool env t) || (returns_bool env f) then
        (*(if (member (APPL (intern_true,[])) (rewrite env (APPL (intern_defined,[t]))))
            &&
            (member (APPL (intern_true,[])) (rewrite env (APPL (intern_defined,[f]))))
         then*)
             [APPL (intern_or,[(APPL (intern_and,[NORMAL c;NORMAL t]));
                         (APPL (intern_and,[(APPL (intern_not,[NORMAL c]));NORMAL f]))])]
         (*else [])*)
    else
        []
  | (APPL (1,[l;r;(APPL (5,[]))])) ->
    [(APPL (intern_trivial,[]))]
  | (APPL (2,[l;r;(APPL (5,[]))])) ->
    [(APPL (intern_trivial,[]))]
  | (APPL (1,[l;r;c])) ->
    if (Match.equal env l r) then
        [(APPL (intern_identity,[]))]
    else
        []
  | (APPL (2,[l;r;c])) ->
    if (Match.equal env l r) then
        [(APPL (intern_identity,[]))]
    else
        []
  | (APPL (13,[VAR x])) -> [(APPL (intern_true,[]))]
  | (APPL (13,[APPL (13,l)])) ->
[APPL (intern_true,[])]
  | (APPL (13,[APPL (f,l)])) ->
    if (Env.isAC env f) && (List.length l)>2 then
        try let (a::r) =l in
        let l2 = [a;(APPL (f,r))] in
        let (e,p) = Env.getFunctionPrecondition env f in
        let theta = first_proper (Match.thematch env e (APPL (f,l2))) in
            if p=NOEXP then
                []
            else
                    [(APPL (intern_and,(Subst.subst theta p)::r))]
        with noProper -> []
    else
        let r = List.map (fun (x) -> (APPL (intern_defined,[x]))) l
        in
            if is_constructor f then
                [(APPL (intern_and,r))]
            else
                (try let (e,p) = Env.getFunctionPrecondition env f in
                let theta = first_proper (Match.thematch env e (APPL (f,l))) in
                    if p=NOEXP then
                        []
                    else
                        [(APPL (intern_and,(Subst.subst theta p)::r))]
                with noProper -> [])
  | (QUANT (15,v,(APPL (17,[e])),p)) ->
    [APPL (intern_not,[QUANT (intern_all,v,e,p)])]
  | (QUANT (14,v,(APPL (17,[e])),p)) ->
[APPL (intern_not,[QUANT (intern_exists,v,(APPL (intern_or,[e;(APPL (intern_not,[p]))])),(APPL (intern_true,[])))])]
  | (QUANT (14,v,(APPL (9,el)),p)) ->
    [(APPL (intern_and,List.map (fun (x) -> (QUANT (intern_all,v,x,p))) el))]
  | (QUANT (15,v,(APPL (10,el)),p)) ->
    [(APPL (intern_or,List.map (fun (x) -> (QUANT (intern_exists,v,x,p))) el))]
  | (QUANT (15,[(vv,t)],APPL (11,[VAR v;e]),p)) ->
    if v=vv && not(List.mem v (Context.getFreeVars e)) then
        [APPL (intern_true,[])]
    else
        []
  | (QUANT (15,[(vv,t)],APPL (11,[e;VAR v]),p)) ->
    if v=vv && not(List.mem v (Context.getFreeVars e)) then
        [APPL (intern_true,[])]
    else
        []
  | (QUANT (15,v,e,p)) ->
    let vars = (Mylist.intersect ((Context.getFreeVars e)@(Context.getFreeVars p)) (strip v))
    in
        if vars=[] then
            [e]
        else if (List.length vars) < (List.length v) then
            [(QUANT (intern_exists,(unstrip vars),e,p))]
        else
            (match e with
               | (APPL (9,l)) ->
                    let eq = find_equal vars [] l
                    in
                        if eq=NOEXP then
                            []
                        else
                            [QUANT (intern_exists,(unstrip vars),Subst.subst (make_subst vars eq) e,p)]
               | _ -> [])
  | (APPL (75,[QUANT (73,[(v,_)],e,_);term])) ->
    [Subst.subst (Subst.addPair Subst.empty v term) e]
  | (QUANT (73,[(v,_)],(APPL (75,[e;v2])),p)) ->
    if (VAR v)=(ExpIntern.decode_one_exp v2) then
        [e]
    else
        []
  | (APPL (f,l)) ->
    if Env.hasSmallerPrecedence env intern_if f then
       (try let (b,(APPL (_,[c;t;e])),a) = first_if [] l
        in
            [(APPL (intern_if,[c;APPL (f,b@[t]@a);APPL (f,b@[e]@a)]))]
        with NoIf -> [])
    else []
  | (QUANT (q,[],e,p)) ->
    if q=intern_all then
        [(APPL (intern_or,[APPL (intern_not,[p]);e]))]
    else if q=intern_exists then
        [e]
    else []
  | (QUANT (q,v,e,p)) ->
    let v1 = remove_quant_vars ((Context.getFreeVars e)@(Context.getFreeVars p)) v
    in
        if v1=v then [] else [(QUANT (q,v1,e,p))]
  | (CASE (CASE (v,t,cl),t2,cl2)) ->
    [(CASE (v,t,
        (List.map (fun (c,e) -> (c,(CASE (e,t2,cl2)))) cl))
    )]
  | (CASE (v,t,cl)) ->
    if cl=[] then
        [(APPL (intern_undef,[]))]
    else if is_finite_clause_list env cl then
       let x = reduce_finite_case v t cl in
           if not(x=[]) then x
           else
              (let tl = List.map (fun (_,x) -> ExpIntern.intern_exp (Env.isACorC env) x) cl in
               let rec all_equal l = match l with
                     | (f::s::r) -> f=s && (all_equal (s::r))
                     | _ -> true in
               let (APPL (f,l),_) = List.hd cl in
               let rec full_list cl =
                       let t = Env.getTypeDefinition env (Type.getetypeName (Type.getReturnetype (Env.getType env f))) in
                       let tc = Type.getConstructorList t in
                       let c = Mylist.remove_dups (List.map (fun (APPL (f,x),_) -> f) cl) in
                           (List.length tc)=(List.length c) in
                   if all_equal cl && full_list cl then
                       [let (_,x) = List.hd cl in x]
                   else
                       let f = find_inversion env (CASE (v,t,cl))
                       in
                           if f=0 then [] else [invert_clauses f (CASE (v,t,cl))]
               )
     else []
  |  x -> []
  ;;



