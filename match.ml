(******************************************************************************
 *  
 * REWRITELIB
 *                            
 * match.ml                       
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
(* require "subst.sml" ;  *)
(* require "context.sml" ;  *)
(* require "trace.sml" ;  *)
(* require "cache.sml" ;  *)
(* require "match-s.sml" ;  *)
(* require "basis.__list" ;  *)
(* require "basis.__integer" ;  *)

(* open listimpl ; *)
(* open ENVimpl ; *)
open Exp ;;
open Intern ;;
(* open EXP_INTERNimpl ; *)
(* open SUBSTimpl ; *)
(* open CONTEXTimpl ; *)
(* open TRACEimpl ; *)
(* open CACHEimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

let rec allInsertions x l = match l with
  | [] -> [[x]]
  | (a::b) ->
    (x::a::b)::
    (List.map (fun (x) -> a::x) (allInsertions x b)) ;;

let rec allPermutations l = match l with
  | [] -> [[]]
  | (a::b) ->
    List.fold_left List.append (List.map
            (fun (x) -> allInsertions a x) ;
            (allPermutations b)) [] ;;

let rec strip l = match l with
  | [] -> []
  | ((a,b)::r) -> a::strip r ;;

let rec pairJoin x y = match (x,y) with
  | ([],[]) -> []
  | ((a::b),(c::d)) -> (a,c)::pairJoin b d ;;

let allMappings list1 list2 =
        List.map
        (pairJoin list1)
        (allPermutations list2) ;;

let rec member1 x l = match l with
  | [] -> false
  | ((a,_)::b) ->
    if x = a then
        true
    else
        member1 x b ;;

let rec match_consts e1 e2 = match (e1,e2) with
  | ((VAR x),(VAR y)) -> [[(x,y)]]
  | ((APPL (s1,l1)),(APPL (s2,l2))) ->
    if s1=s2 && (List.length l1)=(List.length l2) then
        match_consts_list l1 l2
    else []
  | (_,_) -> []
and match_consts_list e1 e2 = match (e1,e2) with
  | ([],[]) -> [[]]
  | ((a::b),(c::d)) ->
       List.fold_left List.append (List.map
           (fun (x) -> List.map (fun l -> x@l) (match_consts_list b d)) ;
           match_consts a c) [] ;;

let rec  depth1 x l = match l with
  | [] -> -1
  | (a::b) ->
    if (member1 x a) then
        0
    else
        let d = depth1 x b in
            if d < 0 then -1 else 1 + d
        ;;

let rec member2 r x l = match l with
  | [] -> false
  | ((r1,a)::b) ->
    if x = a && r = r1 then
        true
    else
        member2 r x b ;;

let rec depth2 r x l = match l with
  | [] -> -1
  | (a::b) ->
    if (member2 r x a) then
        0
    else
        let d = depth2 r x b in
            if d < 0 then -1 else 1 + d
    ;;

let rec member3 r l = match l with
  | [] -> false
  | ((r1,_)::b) ->
    if r = r1 then
        true
    else
        member3 r b ;;

let rec depth3 r l = match l with
  | [] -> -1
  | (a::b) ->
    if (member3 r a) then
        0
    else
        let d = depth3 r b in
            if d < 0 then -1 else 1 + d
        ;;

let rec not_bound a l = match l with
  | [] -> true
  | ([]::r) -> not_bound a r
  | (((_,b)::c)::r) ->
    if a=b then
        false
    else
        not_bound a (c::r) ;;

let rec no_bound_vars l bound = match l with
  | l -> true
  | (a::b) ->
    if not_bound a bound then
        no_bound_vars b bound
    else
        false ;;

let no_bound e bound = no_bound_vars (Context.getFreeVars e) bound ;;

let rec separateVariables q l = match l with
   ((VAR x)::r) ->
    let (vars,terms) = separateVariables q r in
        if (depth1 x q) = -1 then
            (x::vars,terms)
        else
            (vars,(VAR x)::terms)
  | (t::r) ->
    let (vars,terms) = separateVariables q r in
        (vars,t::terms)
  | [] -> ([],[]) ;;

let rec get_all_picks x l = match l with
  | [] -> []
  | (a::b) -> (a,(x@b))::(get_all_picks (a::x) b)

let all_picks x = get_all_picks [] x ;;

let rec all_n_picks r n = match (r,n) with
  | (_,0) -> [([],r)]
  | ([],_) -> []
  | ((a::b),n) ->
    if n > ((List.length b)+1) then
        []
    else
        (List.map (fun (x,y) -> (a::x,y)) (all_n_picks b (n - 1))) @
        (List.map (fun (x,y) -> (x,a::y)) (all_n_picks b n)) ;;

let rec all_splits l = match l with
  | [] -> [([],[])]
  | (a::b) ->
    (List.fold_left List.append []
        (List.map
            (fun (l1,l2) -> [(a::l1,l2);(l1,a::l2)])
            (all_splits b))) ;;

exception NoReduce ;;

let rec makeIndex l exp = match l with
  | [] -> exp
  | ((n,s)::b) -> (INDEX (makeIndex b exp,s,n)) ;;

let rec buildIndices f exp e u = match e with
  | (VAR x) -> Subst.addPair u x (makeIndex f exp)
  | (APPL (s,l)) ->
    let (_,u) =
        List.fold_left (fun (n,u) -> (fun t ->
                    (n + 1,buildIndices ((n,s)::f) exp t u)
                )) (0,u) l
    in u
    ;;

let reduceLet (LET (v,t,e,p)) = Subst.subst (buildIndices [] e v Subst.empty) p ;;

let rec ind n = match n with
  | 0 -> ""
  | n -> (" " ^ (ind (n-1))) ;;

let rec reduceTerms env e s n ee = match ee with
  | ((REF x)::r) ->
    reduceTerms env e s n ((ExpIntern.decode_one_exp (REF x))::r)
  | ((INDEX (e1,s1,n1))::r) ->
    if (u_equal env e e1 []) && s = s1 && n=n1 then
        reduceTerms env e s (n+1) r
    else
        false
  | [] -> true
  | _ -> false
and reduceTerm env r = match r with
  | (REF x) -> reduceTerm env (ExpIntern.decode_one_exp (REF x))
  | (INDEX (REF x,s,i)) -> reduceTerm env (INDEX (ExpIntern.decode_exp (REF x),s,i))
  | (APPL (f,((REF x)::r))) -> reduceTerm env (APPL (f,((ExpIntern.decode_one_exp (REF x))::r)))
  | (INDEX ((APPL (s1,l)),s,i)) ->
    if s1=s then
        List.nth l i
    else
        raise NoReduce
  | (APPL (f,((INDEX (e,s,0))::r))) ->
    if f=s && (reduceTerms env e s 1 r) then
        e
    else
        raise NoReduce
  | x -> raise NoReduce
and recursiveReduceTerm env t = try recursiveReduceTerm env (reduceTerm env t)
                                with NoReduce -> t
and u_equal env t1 t2 q =
    let r1 = recursiveReduceTerm env t1 in
    let r2 = recursiveReduceTerm env t2
in
        uu_equal env r1 r2 q
and uu_equal env e1 e2 q = match (e1,e2) with
  | ((REF x),(REF y)) ->
    if x=y then true
    else if ExpIntern.has_special_construct (REF x) ||
            ExpIntern.has_special_construct (REF y) ||
            not(q=[]) then
        uu_equal env (ExpIntern.decode_one_exp (REF x)) (ExpIntern.decode_one_exp (REF y)) q
    else false
  | ((REF x),y) -> uu_equal env (ExpIntern.decode_one_exp (REF x)) y q
  | (x,(REF y)) -> uu_equal env x (ExpIntern.decode_one_exp (REF y)) q
  | ((NORMAL t1),t2) -> uu_equal env t1 t2 q
  | (t1,(NORMAL t2)) -> uu_equal env t1 t2 q
  | ((VAR v1),(VAR v2)) ->
    let d = depth1 v1 q in
        if d = -1 then
            (if no_bound (VAR v2) q then
                v1 = v2
             else
                false)
        else
            (d = depth2 v1 v2 q)
  | ((MARKED_VAR v1),(MARKED_VAR v2)) ->
    v1=v2
  | ((LET (v1,t1,e1,b1)),e) ->
    u_equal env (reduceLet (LET (v1,t1,e1,b1))) e q
  | (e,(LET (v2,t2,e2,b2))) ->
    u_equal env e (reduceLet (LET (v2,t2,e2,b2))) q
  | ((CASE (e1,t1,c1)),(CASE (e2,t2,c2))) ->
    let l1 = List.map (fun (p,e) -> Subst.subst (buildIndices [] e p Subst.empty) e1) c1 in
    let l2 = List.map (fun (p,e) -> Subst.subst (buildIndices [] e p Subst.empty) e2) c2 in
        (u_equal env e1 e2 q) && (c_equal_pairs env l1 l2 q)
  | ((QUANT (s1,v1,e1,p1)),(QUANT (s2,v2,e2,p2))) ->
    if (s1 = s2) && (List.length v1 = List.length v2) then
            List.mem true (List.map
                              (fun (x) -> u_equal env e1 e2 (x::q) &&
                                          u_equal env p1 p2 (x::q))
                              (allMappings (strip v1) (strip v2)))
    else
        false
  | ((APPL (s,l)),(APPL (s2,l2))) ->
    if s = s2 then
       (if (Env.isAC env s) || (Env.isC env s) then
            c_equal_pairs env l l2 q
        else
            u_equal_pairs env l l2 q)
    else
        false
  | ((INDEX (e1,s1,i1)),(INDEX (e2,s2,i2))) ->
    ((u_equal env e1 e2 q) && s1=s2 && i1=i2)
  | (t1,t2) -> t1=t2
and c_equal_picks env l1 l2 q = match (l1,l2) with
  | ([],[]) -> true
  | (l,(fl::l2)) ->
    equal_eliminate env
    (List.map
        (fun (a,b) -> ((a,b),(fl,l2)))
        (all_picks l)) q
  | (l,[]) -> false
and equal_eliminate env l q = match l with
  | [] -> false
  | (((p1,l1),(p2,l2))::l) ->
    if u_equal env p1 p2 q then
        c_equal_picks env l1 l2 q
    else
        equal_eliminate env l q
and c_equal_pairs env l l2 q =
    ((List.length l)=(List.length l2)) && (c_equal_picks env l l2 q)
and u_equal_pairs env l1 l2 q = match (l1,l2) with
  | ([],[]) -> true
  | ([],_) -> false
  | (_,[]) -> false
  | ((a::b),(c::d)) ->
    (u_equal env a c q) && (u_equal_pairs env b d q) ;;

let equal env p e =
    let _ = Trace.trace "match"
                (fun (xx) -> "equal " ^ (prExp p) ^ " " ^ (prExp e)) in
    let _ = (Trace.indent () ; Trace.undent ()) in
    let res = u_equal env p e []  in
    let _ = Trace.trace "match"
                (fun (xx) -> "equal: " ^ (if res then "Yes" else "No")) in
    let _ = (Trace.indent () ; Trace.undent ())
    in
        res
    ;;

let rec subset_equal env e ee = match ee with
  | (APPL (f,l)) ->
    if Env.isAC env f then
        List.fold_right List.append
            (List.map
                (fun (m,r) ->
                     if List.length m=1 then
                         (if equal env e (List.hd m) then
                             [r]
                          else
                             [])
                     else
                         (if equal env e (APPL (f,m)) then
                             [r]
                          else
                              [])
                )
                (all_splits l)) []
    else
        if equal env e (APPL (f,l)) then
            [[]]
        else
            []
  | _ ->
      if equal env e ee then
          [[]]
      else
          []

let match_unassigned_var env v rest s q theta =
    List.fold_left List.append []
        (List.map (fun (e,r) ->
           if List.length e = 0 then
               (if Env.getID env s = NOEXP then
                    []
                else
                    [(Subst.addPair theta v (Env.getID env s),r)])
           else if List.length e = 1 then
               [(Subst.addPair theta v (List.hd e),r)]
           else
               [(Subst.addPair theta v (APPL (s,e)),r)]) (all_splits rest)) ;;

let rec u_match env t1 t2 q theta = match (t1,t2) with
  | ((REF x),t2) ->
    u_match env (ExpIntern.decode_exp (REF x)) t2 q theta
  | (x,(REF y)) ->
    u_match env x (ExpIntern.decode_exp (REF y)) q theta
  | (t1,(NORMAL t2)) -> u_match env t1 t2 q theta
  | ((VAR v),e) ->
    let d = depth1 v q in
        if d = -1 then
            (if no_bound e q then
                (if (List.mem v (Subst.dom theta)) then
                    List.map
                        (fun (x) -> (theta,x))
                        (subset_equal env (Subst.apply theta v) e)
                else
                    match e with
                      | (APPL (s,l)) -> if (Env.isAC env s) then
                                             match_unassigned_var env
                                                 v l s q theta
                                         else
                                             [(Subst.addPair theta v e,[])]
                      | _             -> [(Subst.addPair theta v e,[])])
            else
                [])
        else
            (match e with
              | (VAR v2) ->
                    if d = depth2 v v2 q then [(theta,[])] else []
              | _ -> [])
  | ((MARKED_VAR v1),(MARKED_VAR v2)) ->
    if not(v1=v2) then
        []
    else
        [(theta,[])]
  | ((MARKED_VAR v1),(VAR v2)) ->
    if not(depth1 v1 q= -1) || not(v1=v2) then
        []
    else
        [(theta,[])]
  | ((LET (v1,t1,e1,b1)),(LET (v2,t2,e2,b2))) ->
    (List.map (fun (x) -> (x,[]))
        (List.fold_left List.append []
            (List.map (fun (t) -> uu_match env e1 e2 q t)
                (List.fold_left List.append []
                    (List.map
                    (fun (x) -> uu_match env b1 b2 (x::q) theta)
                    (match_consts v1 v2))))))
  | ((CASE (e1,t1,c1)),(CASE (e2,t2,c2))) ->
    let permutations = allMappings c1 c2 in
    let matches = List.fold_left List.append [] (List.map
                      (fun (t) -> uu_match env e1 e2 q t)
                      (List.fold_left List.append [] (List.map
                          (fun l -> List.fold_left
                              (fun theta -> (fun ((p1,c1),(p2,c2)) ->
                                  (List.fold_left List.append [] (List.map
                                      (fun (x,theta) -> uu_match env c1 c2 (x::q) theta)
                                      (List.combine (match_consts p1 p2) theta)))
                          )) [theta] l
                          ) permutations)))
    in
        List.map (fun (x) -> (x,[])) matches
  | ((QUANT (s1,v1,e1,p1)),(QUANT (s2,v2,e2,p2))) ->
    if (s1 = s2) && (List.length v1 = List.length v2) then
        (List.fold_left List.append [] (List.map
            (fun (x) -> List.filter
                            (fun (t,r) -> r=[])
                            (List.fold_left List.append [] (List.map
                                (fun (t) -> u_match env p1 p2 (x::q) t)
                                (uu_match env e1 e2 (x::q) theta))))
            (allMappings (strip v1) (strip v2))))
    else
        []
  | ((APPL (s,l)),e) ->
    (match  e with
      | (APPL (s2,l2)) ->
            if s = s2 then
                (if (Env.isAC env s) then
                    ac_match_pairs env s l l2 q theta
                else if (Env.isA env s) then
                    []
                else if (Env.isC env s) then
                    c_match_pairs env l l2 q theta
                else
                    (List.map
                        (fun (t) -> (t,[]))
                        (u_match_pairs env l l2 q theta)))
            else
                []
      | _ ->
            [])
  | (t1,t2) -> if t1=t2 then [(theta,[])] else []
and uu_match env e p q theta =
    (List.map
        (fun (t,r) -> t)
        (List.filter
            (fun (t,r) -> r = [])
            (u_match env e p q theta)))
and c_match_pairs env l l2 q theta =
    if (List.length l)=(List.length l2) then
        (List.map
            (fun (x) -> (x,[]))
            (List.fold_left List.append [] (List.map
                (fun (x) -> match_mapping env x q theta)
                (allMappings l l2))))
    else
        []
and match_mapping env l q theta = match l with
  | [] -> [theta]
  | ((t1,t2)::b) ->
    List.fold_left List.append [] (List.map
        (fun (t) -> match_mapping env b q t)
        (uu_match env t1 t2 q theta))
and ac_match_pairs env s l l2 q theta =
    let (vars,terms) = (separateVariables q l) in
        (List.fold_left List.append [] (List.map
            (fun (theta,rest) -> if vars=[]
                                 then [(theta,rest)]
                                 else match_vars env vars rest s q theta)
            (match_terms env terms l2 q theta)))
and match_vars env vars rest s q theta =
    if (List.length vars)=(List.length rest) then
        match_single_vars env vars rest s q theta
    else if (List.length vars)<(List.length rest) then
        (List.fold_left List.append [] (List.map
            (fun (p,r) ->
            (List.fold_left List.append [] (List.map
                (fun (theta,rest) -> match_single_vars env r rest s q theta)
                (match_multiple_var env p rest ((List.length rest)-(List.length r)) s q theta)
            )))
            (all_picks vars))
         )@
         (match_single_vars env vars rest s q theta)
    else
        []
and match_multiple_var env v terms n s q theta =
    if n=1 then
        match_single_var env v terms s q theta
    else
        (if List.mem v (Subst.dom theta) then
             []
         else
             (List.map
                 (fun (p,r) -> (Subst.addPair theta v (APPL (s,p)),r))
                 (all_n_picks terms n)))
and match_single_vars env l rest s q theta = match l with
  | [] -> [(theta,rest)]
  | (a::b) ->
       (List.fold_left List.append [] (List.map
       (fun (t,rest) -> match_single_vars env b rest s q t)
       (match_single_var env a rest s q theta)))
and match_single_var env v rest s q theta =
            (List.fold_left List.append [] (List.map (fun (t) ->
                (
                    (List.map
                        (fun (tt,r) -> (tt,Mylist.delete_one t rest))
                        (List.filter (fun (t,r) -> r=[])
                            (u_match env (VAR v) t q theta)))
                )) rest
            ))
and match_terms env l l2 q theta = match l with
  | [] -> [(theta,l2)]
  | (a::b) ->
    (List.fold_left List.append [] (List.map
        (fun (theta,rest) -> match_terms env b rest q theta)
        (match_term env a l2 q theta)))
and match_term env term l q theta = match l with
  | [] -> []
  | (a::b) ->
    (List.map
        (fun (theta) -> (theta,b))
        (uu_match env term a q theta))@
     (List.map
         (fun (theta,rest) -> (theta,a::rest))
         (match_term env term b q theta))
and u_match_pairs env l1 l2 q theta = match (l1,l2) with
  | ([],[]) -> [theta]
  | ((a::b),[]) -> []
  | ([],(a::b)) -> []
  | ((a::b),(c::d)) ->
       List.fold_left List.append [] (List.map
           (fun (t) -> u_match_pairs env b d q t)
           (uu_match env a c q theta)) ;;

let rec thematch env p e = match (p,e) with
  | ((REF p),(REF e)) ->
   (try (Cache.get_match p e) with Cache.NoEntry ->
    let r = thematch env (ExpIntern.decode_exp (REF p)) (ExpIntern.decode_exp (REF e))
    in
        Cache.add_match p e r ;
        r)
  | (p,e) ->
    let _ = Trace.trace "rewriteRule" (fun (x) -> "match " ^ (prExp p) ^ " " ^ (prExp e)) in
    let _ = Trace.indent () in
    let res = u_match env p e [] Subst.empty in
    let _ = Trace.undent () in
    let _ = Trace.trace "rewriteRule" (fun (x) -> "end match " ^ (prExp p) ^ " " ^ (prExp e)) in
        res
    ;;

let rec swap l = match l with
  | [] -> []
  | (f::r) ->
    (List.map (fun (a,b) -> (b,a)) f)::(swap r) ;;

let rec u_unify env t1 t2 q theta1 theta2 = match (t1,t2) with
  | ((REF x),t2) ->
    u_unify env (ExpIntern.decode_exp (REF x)) t2 q theta1 theta2
  | (x,(REF y)) ->
    u_unify env x (ExpIntern.decode_exp (REF y)) q theta1 theta2
  | (t1,(NORMAL t2)) -> u_unify env t1 t2 q theta1 theta2
  | ((VAR v1),(VAR v2)) ->
    let d1 = depth1 v1 q in
    let d2 = depth3 v2 q in
        if d1 = -1 && d2= -1 then
           (if (List.mem v1 (Subst.dom theta1)) then
                List.map (fun (t2,r) -> (theta1,t2,r,[]))
                    (u_match env (VAR v2) (Subst.apply theta1 v1) (swap q) theta2)
            else if (List.mem v2 (Subst.dom theta2)) then
                List.map (fun (t1,r) -> (t1,theta2,[],r))
                    (u_match env (VAR v1) (Subst.apply theta2 v2) q theta1)
            else
                [(Subst.addPair theta1 v1 (VAR v2),Subst.addPair theta2 v2 (VAR v2),[],[])])
        else
            if d1 = depth2 v1 v2 q then [(theta1,theta2,[],[])] else []
  | ((VAR v),e) ->
    let d = depth1 v q in
        if d = -1 then
            (if no_bound e q then
                (if (List.mem v (Subst.dom theta1)) then
                    List.map
                        (fun (x) -> (theta1,theta2,x,[]))
                        (subset_equal env (Subst.apply theta1 v) e)
                else
                    match e with
                      | (APPL (s,l)) -> if (Env.isAC env s) then
                                             (List.map
                                                 (fun (theta1,x) -> (theta1,theta2,x,[]))
                                                 (match_unassigned_var env
                                                 v l s q theta1))
                                         else
                                             [(Subst.addPair theta1 v e,theta2,[],[])]
                      | _             -> [(Subst.addPair theta1 v e,theta2,[],[])])
            else
                [])
        else []
  | (e,(VAR v)) ->
    let d = depth3 v q in
        if d = -1 then
            (if no_bound e q then
                (if (List.mem v (Subst.dom theta2)) then
                    List.map
                        (fun (x) -> (theta1,theta2,[],x))
                        (subset_equal env (Subst.apply theta2 v) e)
                else
                    match e with
                      | (APPL (s,l)) -> if (Env.isAC env s) then
                                             (List.map
                                                 (fun (theta2,x) -> (theta1,theta2,[],x))
                                                 (match_unassigned_var env
                                                 v l s q theta2))
                                         else
                                             [(theta1,Subst.addPair theta2 v e,[],[])]
                      | _             -> [(theta1,Subst.addPair theta2 v e,[],[])])
            else
                [])
        else []
  | ((MARKED_VAR v1),(MARKED_VAR v2)) ->
    if not(v1=v2) then
        []
    else
        [(theta1,theta2,[],[])]
  | ((MARKED_VAR v1),(VAR v2)) ->
    if not(depth1 v1 q= -1) || not(v1=v2) then
        []
    else
        [(theta1,theta2,[],[])]
  | ((LET (v1,t1,e1,b1)),(LET (v2,t2,e2,b2))) ->
    (List.map
        (fun (t1,t2) -> (t1,t2,[],[]))
        (List.fold_left List.append [] (List.map (fun (t1,t2) -> uu_unify env e1 e2 q t1 t2)
        (List.fold_left List.append [] (List.map (fun (x) -> uu_unify env b1 b2 (x::q) theta1 theta2)
        (match_consts v1 v2))))))
  | ((CASE (e1,t1,c1)),(CASE (e2,t2,c2))) ->
    let permutations = allMappings c1 c2 in
    let matches = List.fold_left List.append [] (List.map
            (fun (t1,t2) -> uu_unify env e1 e2 q t1 t2)
            (List.fold_left List.append [] (List.map
            (fun (l) -> List.fold_left
                 (fun thetal -> (fun ((p1,c1),(p2,c2)) ->
                     (List.fold_left List.append [] (List.map (fun (x,(theta1,theta2)) -> uu_unify env c1 c2 (x::q) theta1 theta2)
                     (List.combine (match_consts p1 p2) thetal)))
                 )) [(theta1,theta2)] l
            )
            permutations)))

    in
        List.map
            (fun (t1,t2) -> (t1,t2,[],[]))
            matches
  | ((QUANT (s1,v1,e1,p1)),(QUANT (s2,v2,e2,p2))) ->
    if (s1 = s2) && (List.length v1 = List.length v2) then
         List.fold_left List.append [] (List.map
            (fun (x) ->
                       List.filter
                           (fun (t1,t2,r1,r2) -> r1=[] && r2=[])
                           (List.fold_left List.append [] (List.map
                               (fun (t1,t2) -> u_unify env p1 p2 (x::q) t1 t2)
                               (uu_unify env e1 e2 (x::q) theta1 theta2))))
            (allMappings (strip v1) (strip v2)))
    else
        []
  | ((APPL (s,l)),(APPL (s2,l2))) ->
    if s = s2 then
       (if (Env.isAC env s) then
            ac_unify_pairs env s l l2 q theta1 theta2
        else if (Env.isA env s) then
            []
        else if (Env.isC env s) then
            c_unify_pairs env l l2 q theta1 theta2
        else
            (List.map
                (fun (t1,t2) -> (t1,t2,[],[]))
                (u_unify_pairs env l l2 q theta1 theta2)))
    else
        []
  | (t1,t2) -> if t1=t2 then [(theta1,theta2,[],[])] else []
and uu_unify env e p q theta1 theta2 =
    List.map
        (fun (t1,t2,r1,r2) -> (t1,t2))
        (List.filter
            (fun (t1,t2,r1,r2) -> r1 = [] && r2=[])
            (u_unify env e p q theta1 theta2))
and c_unify_pairs env l l2 q theta1 theta2 =
    if (List.length l)=(List.length l2) then
        (List.map
            (fun (x,y) -> (x,y,[],[]))
            (List.fold_left List.append [] (List.map
                (fun (x) -> unify_mapping env x q theta1 theta2)
                (allMappings l l2))))
    else
        []
and unify_mapping env l q theta1 theta2 = match l with
  | [] -> [(theta1,theta2)]
  | ((t1,t2)::b) ->
    (List.fold_left List.append [] (List.map
        (fun (t1,t2) -> unify_mapping env b q t1 t2)
        (uu_unify env t1 t2 q theta1 theta2)))
and ac_unify_pairs env s l l2 q theta1 theta2 =
    let (vars1,terms1) = separateVariables q l in
    let (vars2,terms2) = separateVariables q l2 in
        (List.fold_left List.append [] (List.map
            (fun (theta1,theta2,rest1,rest2) -> if vars1=[] && vars2=[]
                            then [(theta1,theta2,rest1,rest2)]
                            else unify_vars env vars1 rest1 vars2 rest2 s q theta1 theta2)
            (unify_terms env terms1 terms2 q theta1 theta2)))
and unify_vars env vars1 rest1 vars2 rest2 s q theta1 theta2 =
    let v1=List.map (fun (x) -> VAR x) vars1 in
    let v2=List.map (fun (x) -> VAR x) vars2 in
        (*val _ = print "unify vars\n"
    let _ = List.map (fun (x) -> print ("v1 " ^ (prExp x) ^ "\n")) v1 in
    let _ = List.map (fun (x) => print ("v2 " ^ (prExp x) ^ "\n")) v2 in
    let _ = List.map (fUn (x) => print ("r1 " ^ (prExp x) ^ "\n")) rest1 in
    let _ = List.map (fun (x) => print ("r2 " ^ (prExp x) ^ "\n")) rest2*)
        if (List.length vars1)+(List.length rest1)=(List.length vars2)+(List.length rest2) then
            unify_single_vars1 env (v1@rest1) (v2@rest2) s q theta1 theta2
        else if (List.length vars1)+(List.length rest1)<(List.length vars2)+(List.length rest2) then
            (List.fold_left List.append [] (List.map
                (fun (p,r) ->
                    (List.fold_left List.append [] (List.map
                        (fun (theta1,theta2,rest,_) -> unify_single_vars1 env r rest s q theta1 theta2)
                            (unify_multiple_var1 env p (rest2@v2) ((List.length rest2)+(List.length vars2)-(List.length rest1)-(List.length vars1)+1) s q theta1 theta2))))
(all_picks v1)))@
             (unify_single_vars1 env (v1@rest1) (v2@rest2) s q theta1 theta2)
        else
            (List.fold_left List.append [] (List.map
                (fun (p,r) ->
                    (List.fold_left List.append [] (List.map
                        (fun (theta1,theta2,_,rest) -> unify_single_vars2 env rest r s q theta1 theta2)
                             (unify_multiple_var2 env (rest1@v1) p ((List.length rest1)+(List.length vars1)-(List.length rest2)-(List.length vars2)+1) s q
                                      theta1 theta2)
                    )))
                (all_picks v2)))@
             (unify_single_vars2 env (v1@rest1) (v2@rest2) s q theta1 theta2)
and unify_multiple_var1 env v terms n s q theta1 theta2 =
    let (VAR vv) = v
        (*val _ = print "mv1\n"
        val _ = print ("v " ^ (prExp v) ^ "\n")
        val _ = map (fn (x) => print ("t " ^ (prExp x) ^ "\n")) terms*)
    in
        if n=1 then
            unify_single_var1 env v terms s q theta1 theta2
        else
            (if List.mem vv (Subst.dom theta1) then
                 []
             else
                 (List.map
                     (fun (p,r) -> (Subst.addPair theta1 vv (APPL (s,p)),theta2,r,[]))
                     (all_n_picks terms n)))
and unify_single_vars1 env l rest s q theta1 theta2 = match l with
  | [] -> [(theta1,theta2,rest,[])]
  | (a::b) ->
       (List.fold_left List.append [] (List.map
           (fun (t1,t2,rest,_) -> unify_single_vars1 env b rest s q t1 t2)
           (unify_single_var1 env a rest s q theta1 theta2)))
and unify_single_var1 env v rest s q theta1 theta2 =
    (List.fold_left List.append [] (List.map (fun (t) ->
        (List.map
            (fun (t1,t2,r1,r2) -> (t1,t2,Mylist.delete_one t rest,[]))
            (List.filter
                (fun (t1,t2,r1,r2) -> r1=[] && r2=[])
                (u_unify env v t q theta1 theta2))
            )) rest))
and unify_multiple_var2 env terms v n s q theta1 theta2 =
    let (VAR vv) = v
        (*val _ = print "mv2\n"
        val _ = print ("v " ^ (prExp v) ^ "\n")
        val _ = map (fn (x) => print ("t " ^ (prExp x) ^ "\n")) terms*)
    in
        if n=1 then
            unify_single_var2 env terms v s q theta1 theta2
        else
            (if List.mem vv (Subst.dom theta2) then
                 []
             else
                 (List.map
                     (fun (p,r) -> (theta1,Subst.addPair theta2 vv (APPL (s,p)),[],r))
                     (all_n_picks terms n)))
and unify_single_vars2 env rest l s q theta1 theta2 = match l with
  | [] -> [(theta1,theta2,[],rest)]
  | (a::b) ->
        (List.fold_left List.append [] (List.map
            (fun (t1,t2,_,rest) -> unify_single_vars2 env rest b s q t1 t2)
            (unify_single_var2 env rest a s q theta1 theta2)))
and unify_single_var2 env rest v s q theta1 theta2 =
    (List.fold_left List.append [] (List.map
        (fun (t) ->
            (List.map
                (fun (t1,t2,r1,r2) -> (t1,t2,[],Mylist.delete_one t rest))
                (List.filter
                    (fun (t1,t2,r1,r2) -> r1=[] && r2=[])
                    (u_unify env t v q theta1 theta2)))) rest))
and unify_terms env l l2 q theta1 theta2 = match (l,l2) with
  | ([],_) -> [(theta1,theta2,l2,[])]
  | (l1,_) -> [(theta1,theta2,[],l1)]
  | ((a::b),l2) ->
    (List.fold_left List.append [] (List.map
        (fun (theta1,theta2,rest) -> unify_terms env b rest q theta1 theta2)
        (unify_term env a l2 q theta1 theta2)))
and unify_term env term l q theta1 theta2 = match l with
  | [] -> []
  | (a::b) ->
    (List.map
        (fun (theta1,theta2) -> (theta1,theta2,b))
        (uu_unify env term a q theta1 theta2)) @
    (List.map
        (fun (theta1,theta2,rest) -> (theta1,theta2,a::rest)))
        (unify_term env term b q theta1 theta2)
and u_unify_pairs env l1 l2 q theta1 theta2 = match (l1,l2) with
  | ([],[]) -> [(theta1,theta2)]
  | ((a::b),[]) -> []
  | ([],(a::b)) -> []
  | ((a::b),(c::d)) ->
       (List.fold_left List.append [] (List.map
           (fun (t1,t2) -> u_unify_pairs env b d q t1 t2)
           (uu_unify env a c q theta1 theta2))) ;;

let rec unify env p e = match (p,e) with
  | ((REF p),(REF e)) ->
   (try (Cache.get_unify p e) with Cache.NoEntry ->
    let r = unify env (ExpIntern.decode_exp (REF p)) (ExpIntern.decode_exp (REF e))
    in
        Cache.add_unify p e r ;
        r
    )
  | (p,e) ->
    let _ = Trace.trace "rewriteRule" (fun (x) -> "unify " ^ (prExp p) ^ " " ^ (prExp e)) in
    let _ = Trace.indent () in
    let res = u_unify env p e [] Subst.empty Subst.empty in
    let _ = Trace.undent () in
    let _ = Trace.trace "rewriteRule" (fun (x) -> "end unify " ^ (prExp p) ^ " " ^ (prExp e))
    in
        res
    ;;

let rec all_smaller env l s = match l with
  | [] -> true
  | (a::b) -> (Env.hasSmallerPrecedence env a s) &&
    (all_smaller env b s)
  ;;

let rec all_smaller_list env l l2 = match l2 with
  | [] -> false
  | (a::b) -> (all_smaller env l a) || (all_smaller_list env l b)
  ;;

let rec smaller_list env f l = match l with
  | [] -> true
  | (a::b) ->
    (Env.hasSmallerPrecedence env f a) && (smaller_list env f b)
  ;;

let rec member_eq env x l = match l with
  | [] -> false
  | (a::b) -> equal env x a && member_eq env x b ;;

let rec has_forbidden env t =
    hf env t []
and hf env t bound =
    (Mylist.intersect (Context.getFreeVars t) bound)=[] &&
    member_eq env t (Env.getForbids env) || hfs env t bound
and hfs env e bound = match e with
  | (APPL (f,l)) ->
    let rec h l = match l with
          | [] -> false
          | (a::b) -> hf env a bound || h b
    in
        h l
  | (QUANT (q,t,e,p)) ->
    let bound = bound@(List.map (fun (v,_) -> v) t)
    in
        hf env e bound || hf env p bound
    ;;

let rec much_smaller env e1 e2 =
    if has_forbidden env e1 then
        false
    else if has_forbidden env e2 then
        true
    else
        all_smaller_list env (allSymbols e1) (allSymbols e2)
  ;;

let rec remove_ed env a l = match l with
  | [] -> []
  | (b::c) ->
    if equal env a b then
        remove_ed env a c
    else
        b::(remove_ed env a c) ;;

let rec remove_equal_dups env l = match l with
  | [] -> []
  | (a::b) ->
    ((Trace.trace "rewriteRule" (fun (xx) -> "equal_dups " ^ (string_of_int (List.length (a::b))))) ;
    a::(remove_equal_dups env (remove_ed env a b)))
  ;;

let rec swallow_symbols env s exps t =
    let (*val _ = Trace.trace "match" (fn (x) => ("Breaking " ^ (decode s) ^ " " ^ (prExp t)))
    let _ = Trace.trace_list "match" (fn (x) => (map prExp exps))*)
        matches = List.map
                          (fun (a,b) -> List.map (Subst.apply a) (Subst.dom a))
                          (List.filter
                              (fun (a,b) -> b=[])
                              (List.fold_left List.append [] (List.map
                                  (fun (x) -> thematch env x t) exps))) in
        (*val _ = Trace.trace "match" (fn (x) => "Matches:")
    let _ = Trace.trace_list "match" (fn (x) => (map prExp (foldr append [] matches)))*)
        if matches=[] then [t] else List.fold_right List.append (List.map (swallow_symbols env s exps) (List.hd matches)) []
    ;;

let rec pair_map env s ex  l = match (ex,l) with
  | (_,[]) -> l
  | (ex,[]) -> []
  | ((a::b),(c::d)) -> (swallow_symbols env s a c)@(pair_map env s b d)
  ;;

let rec extend_list l n = match (l,n) with
  | ((a::b),n) -> a::(extend_list b (n-1))
  | (x,0) -> x
  | ([],n) -> []::(extend_list [] (n-1))
  ;;

let map_list env s l f =
    let ex = Env.getExpanders env s in
    let ex2 = if f>0 then Env.filterList env s (extend_list ex f) else ex
    in
        pair_map env s ex2 l
    ;;

let rec equal_smaller env x y =
    if member_eq env x (Env.getForbids env) then
        false
    else if member_eq env y (Env.getForbids env) then
        true
    else es env x y
and es env p e = match (p,e) with
  | ((VAR x),_) -> List.mem x (Context.getFreeVars e)
  | ((MARKED_VAR x),_) ->
    if (Context.getFreeVars e)=[] then
        List.mem x (Context.getMarkedVars e)
    else
        true
  | ((NUM n),(APPL (s,l))) -> true
  | ((NUM n),(NUM m)) -> not(m<n)
  | ((CHAR n),(APPL (s,l))) -> true
  | ((CHAR n),(CHAR m)) -> not(m<n)
  | ((STRING n),(APPL (s,l))) -> true
  | ((STRING n),(STRING m)) -> not(m<n)
  | ((NUM n),(VAR v)) -> true
  | ((CHAR n),(VAR v)) -> true
  | ((STRING s),(VAR v)) -> true
  | ((APPL (s1,ll1)),(APPL (s2,ll2))) ->
    let _ = Trace.trace "match" (fun (x) -> "comparing " ^ (prExp (APPL (s1,ll1))) ^ " " ^ (prExp (APPL (s2,ll2)))) in
    let l1=map_list env s1 (Env.filterList env s1 ll1) (List.length ll1) in
    let l2=map_list env s2 (Env.filterList env s2 ll2) (List.length ll2) in
    let res=if (Env.hasEqualPrecedence env s1 s2) then
                    not(List.mem false (List.map (fun (x) -> equal_smaller_than_list env x l2) l1))
                else if (Env.hasSmallerPrecedence env s1 s2) then
                    all_smaller env l1 (APPL (s2,l2))
                else
                    equal_smaller_than_list env (APPL (s1,l1)) l2
    in
        if res || (l1=ll1 && l2=ll2) then
            res
        else
            let res2=if (Env.hasEqualPrecedence env s1 s2) then
                         not(List.mem false (List.map (fun (x) -> equal_smaller_than_list env x l1) l2))
                     else if (Env.hasSmallerPrecedence env s2 s1) then
                         all_smaller env l2 (APPL (s1,l1))
                     else
                         equal_smaller_than_list env (APPL (s2,l2)) l1
            in
                if res2 then false
                else
                    let l1=map_list env s1 ll1 0 in
                    let l2=map_list env s2 ll2 0 in
                        if (Env.hasEqualPrecedence env s1 s2) then
                            not(List.mem false (List.map (fun (x) -> equal_smaller_than_list env x l2) l1))
                        else if (Env.hasSmallerPrecedence env s1 s2) then
                            all_smaller env l1 (APPL (s2,l2))
                        else
                            equal_smaller_than_list env (APPL (s1,l1)) l2
  | ((REF x),y) -> equal_smaller env (ExpIntern.decode_exp (REF x)) y
  | (x,(REF y)) -> equal_smaller env x (ExpIntern.decode_exp (REF y))
  | (_,_) -> false
and smaller_equal_pairs env l1 l2 = match (l1,l2) with
  | ([],[]) -> true
  | ((a::b),(c::d)) ->
    (equal_smaller env a c) && (smaller_equal_pairs env b d)
and all_smaller env l x = match l with
  | [] -> true
  | (a::b) ->
    (equal_smaller env a x) &&
    (all_smaller env b x) && not(equal env a x)
and all_smaller_equal env l x = match l with
  | [] -> true
  | (a::b) ->
    (equal_smaller env a x) && (all_smaller_equal env b x)
and equal_smaller_than_list env a l = match l with
  | [] -> false
  | (b::c) ->
    (equal_smaller env a b) || (equal_smaller_than_list env a c)
and smaller_than_list env a l = match l with
  | [] -> false
  | (b::c) ->
    ((equal_smaller env a b) && not(equal env a b))
    || (smaller_than_list env a c) ;;

