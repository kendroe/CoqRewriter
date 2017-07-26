(******************************************************************************
 *                                   
 * REWRITELIB                        
 *                                   
 * type.ml                        
 *                                   
 * This file contains the implementation of the TYPE module
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
(* require "parser.sml" ;  *)
(* require "lex.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "type-s.sml" ;  *)
(* require "basis.__list" ;  *)
(* require "basis.__integer" ;  *)
(* require "basis.__substring" ;  *)

open Mylist ;;
open Parser ;;
open Lex ;;
open Intern ;;

(* infix 3 |||| ; *)
(* infix 4 ||| ; *)
(* infix 5 ==> ; *)
(* infix 6 & ; *)

type tyvar = Var of int ;;
type etype = V of tyvar
           | S of tyvar
           | F of etype * etype
           | P of int * etype list
           | N ;;

let varCount = ref 0 ;; (* Count used in assigning new type variables *)
type typeDef = (int * (etype list)) list ;;

exception TypeError of (etype * etype * (int list)) ;;
(*
 * Parsing/unparsing
 *)
let unparsev (Var x) = if x < 26 then "'" ^ String.make 1 (char_of_int (x + 97)) else
                       "'" ^ (string_of_int x) ;;

let rec unparse t = match t with
  | (V v) -> unparsev v
  | (S v) -> unparsev v
  | (F (t1,t2)) -> "(" ^ (unparse t1) ^ ") -> (" ^
                                 (unparse t2) ^ ")"
  | (P (s,[])) -> decode s
  | (P (46,(f::r))) -> (unparse f) ^ (List.fold_right ( ^ ) (List.map (fun (x) -> (" * " ^ (unparse x))) r) "")
  | (P (s,sl)) -> (decode s) ^ "(" ^ (unparsel sl) ^ ")"
  | N -> "**NOTYPE**"
and unparsel t = match t with
  | [] -> ""
  | [a] -> unparse a
  | (a::b) -> (unparse a) ^ ", " ^ (unparsel b) ;;

let sord x = if x=="" then 0 else (int_of_char (String.get x 0));;

let parseVar = (((getToken (SPECIAL "'")) & (getToken (ID "")))
                     ==> (fun (x,ID y) -> Var ((sord y) - 97)))
             ||| ((getToken (SPECIAL "'") & getToken (NUMBER 0))
                     ==> (fun (x,NUMBER y) -> Var y)) ;;

let prec = [RIGHT (SYMBOL "->",1); RIGHT (SYMBOL "*",2)] ;;

let rec parseBasic l =  (parseVar ==> (fun (x) -> V(x))
                 ||| (((getToken (SPECIAL "(") & parseetype) &
                     getToken (SPECIAL ")"))
                     ==> (fun ((a,b),c) -> b))) l
and parseList l =  ((getToken (SPECIAL ")") ==> (fun (x) -> []))
                ||| (((getToken (SPECIAL ",") & parseetype) &
                    parseList) ==> (fun ((x,y),z) -> (y::z)))) l
and parseFlist l = ((((getToken (SPECIAL "(") & parseetype) & parseList)
                     ==> (fun ((a,b),c) -> b::c))) l
and parseProd l =  (parseBasic
                ||| ((getToken (ID "") & parseFlist)
                    ==> (fun (ID a,b) -> P(intern a,b)))
                ||| (getToken (ID "") ==> (fun (ID x) -> P(intern x,[])))) l
and conv x t y = match x,t,y with
  | (x,(SYMBOL "->"),y) -> F (x,y)
  | ((P(46,l1)),(SYMBOL "*"),(P (46,l2))) -> P (intern_t,l1@l2)
  | (x,(SYMBOL "*"),(P (46,l))) -> P (intern_t,x::l)
  | ((P(46,l)),(SYMBOL "*"),y) -> P (intern_t,l@[y])
  | (x,(SYMBOL "*"),y) -> (P (intern_t,[x;y]))
and parseetype l = precParse prec parseProd conv l;;

let parse x = let ((a,b)::c) = parseetype (tokenize x) in b ;;

let rec unparseList l = match l with
  | [x] -> unparse x
  | (a::b) -> (unparse a) ^ "," ^ (unparseList b) ;;

let rec unparseItem i = match i with
  | (s,a::b) -> (decode s) ^ "(" ^ (unparseList (a::b)) ^ ")"
  | (s,[]) -> (decode s)
and unparseDef d = match d with
  | [] -> ""
  | [x] -> unparseItem x
  | (a::b) -> (unparseItem a) ^ " | " ^ (unparseDef b) ;;

let rec parseetypeList l = ((((parseetype & getToken (SPECIAL ",")) &
                       parseetypeList) ==> (fun ((x,y),z) -> x::z))
                   ||| (parseetype ==> (fun (x) -> [x]))) l ;;
let parseDefItem l = (((((getToken (ID "") & getToken (SPECIAL "(")) &
                      parseetypeList) & getToken (SPECIAL ")"))
                          ==> (fun (((ID a,b),c),d) -> (intern a,c)))
                ||| (getToken (ID "") ==> (fun (ID x) -> (intern x,[])))) l ;;
let rec parseDefList l = (((parseDefItem & getToken (SYMBOL "|")) &
                      parseDefList) ==> (fun ((x,y),z) -> x::z)
                  ||| (parseDefItem ==> (fun (x) -> [x]))) l ;;
let parseDef x =
    let ((a,b)::c) = parseDefList (tokenize x) in b ;;

let rec parseStringList l =  ((getToken (SPECIAL ")") ==> (fun (x) -> []))
                     ||| (((getToken (SPECIAL ",") & getToken (ID "")) &
                         parseStringList) ==>
                         (fun ((x,ID y),z) -> (intern y::z)))) l ;;

let parseLeft l = (((((getToken (ID "") & getToken (SPECIAL "(")) &
                   getToken (ID "")) & parseStringList) ==>
                   (fun ((((ID x),a),ID b),c) -> (intern x,intern b::c)))
               ||| (getToken (ID "") ==> (fun (ID x) -> (intern x,[])))) l ;;

let parseWholeD x = (((parseetype & getToken (SYMBOL "=")) & parseDefList)
            ==> (fun ((t,m),r) -> (t,r))) x ;;
let parseWholeDef x =
    let ((a,b)::c) = parseWholeD (tokenize x) in b ;;

(*
 * Constructor/destructor functions
 *)
let newVar () = (varCount := !varCount + 1; Var(!varCount)) ;;
let mkVar v = V(v) ;;
let untypeVar s v = match v with
  | (V(x)) -> if s = x then x else
    raise (TypeError (V(x),V(x),[]))
  | x -> raise (TypeError (x,x,[])) ;;
let mkSlot v = S(v) ;;
let untypeSlot t = match t with
  | (S(x)) -> x
  | t -> raise (TypeError (t,t,[]));;
let mkProduct s tl = P(s,tl) ;;
let untypeProduct s n p = match p with
  | (P(x,tl)) ->
        if s = x && (List.length tl) = n then tl else
        raise (TypeError (P(x,tl),P(x,tl),[]))
  | t -> raise (TypeError (t,t,[]));;
let mkTfun t1 t2 = F(t1,t2) ;;
let untypeTfun t = match t with
  | (F(t1,t2)) -> (t1,t2)
  | t -> raise (TypeError(t,t,[]));;
let notype = N ;;

let getetypeName (P (n,tl)) = n ;;

let rec allNames t = (match t with
  | (P (n,tl)) ->
    n::(List.fold_right List.append (List.map (fun (x) -> allNames x) tl) [])
  | (F (t1,t2)) -> (allNames t1)@(allNames t2)
  | x -> [])
  ;;

let allDefinitionNames dl =
    List.fold_right List.append
         (List.map (fun (s,tl) ->
                  List.fold_right List.append (List.map allNames tl) []
              )
              dl
         ) []
  ;;

(*
 * Matching
 *)
type subst = Subst of ((tyvar * etype) list) ;;
exception NoMapping ;;

let rec printSubst s = match s with
  | (Subst []) -> (print_string "\n")
  | (Subst ((a,b)::c)) ->
    (print_string ((unparsev a) ^ " -> " ^ (unparse b) ^ "\n") ;
     printSubst (Subst c))
  ;;

let id = Subst([]) ;;

let rec get sl x = match sl with
  | (Subst((v,t)::r)) -> if v = x then t else get (Subst(r)) x
  | (Subst []) -> raise NoMapping ;;

let rec on t s = match t with
  | (S x) -> (S x)
  | (V x) -> (try get s x with NoMapping -> (V x))
  | (F (l,r)) -> F ((on l s),(on r s))
  | (P (n,l)) -> P (n,(List.map (fun x -> on x s) l))
  | N -> N ;;

let rec rec_on t s =
    let t2 = on t s in
        if t = t2 then t else rec_on t2 s
    ;;

let rec map_sub s a b = match s with
  | (Subst []) -> (Subst [])
  | (Subst ((tv,t)::r)) ->
    let Subst rr = map_sub (Subst r) a b in
        Subst ((tv, on t (Subst [(a,b)]))::rr)
    ;;

let addSubst (Subst(x)) a b = let (Subst s2) = map_sub (Subst x) a b in
                                  Subst((a,b)::s2)
                              ;;

let rec pairLists l1 l2 = match (l1,l2) with
  | ([],[]) -> []
  | ((a::b),(c::d)) -> ((a,c)::pairLists b d)
  | (_,_) -> raise (TypeError(N,N,[])) ;;

let rec tmatch t e s = match (t,e) with
  | ((V x),e) -> (try (tmatch (get s x) e s)
    with NoMapping -> addSubst s x e)
  | ((S x),e) -> (untypeVar x e ; s)
  | ((P (i,l)),e) ->
    let (m,_) =
        List.fold_left (fun (s,n) -> (fun (p,e) ->
                  ((try tmatch p e s with (TypeError(t1,t2,l)) ->
                  raise (TypeError(t1,t2,n::l))),
                  n + 1)
             ))
             (s,0)
             (pairLists l (untypeProduct i (List.length l) e))
    in
        m
  | ((F (p1,p2)),e) ->
      let (e1,e2) = untypeTfun e in
      let s1 = (try tmatch p1 e1 s with TypeError(t1,t2,l) ->
                   raise (TypeError(t1,t2,0::l))) in
          (try tmatch p2 e2 s1 with TypeError(t1,t2,l) ->
          raise (TypeError(t1,t2,1::l)))
  | (N,N) -> s
  | (N,t) -> raise (TypeError(N,t,[])) ;;

let pretype x = unparse x ;;

let notetype t = match t with
  | N -> true
  | _ -> false ;;

let rec all_variables t = match t with
  | (V(v)) -> [v]
  | (S(v)) -> [v]
  | (P(s,vl)) -> List.fold_right List.append (List.map (fun (x) -> all_variables x) vl) []
  | (F(l,r)) -> (all_variables l) @ (all_variables r);;

let rec int_unify s t1 t2 = match (t1,t2) with
  | ((V x),(V y)) ->
    if x = y then
        s
    else let vx = try get s x with NoMapping -> (V x) in
         let vy = try get s y with NoMapping -> (V y) in
       if vx = (V x) && vy = (V y) then
           addSubst s x (V y)
       else
           int_unify s vx vy
  | ((V x),y) ->
    if member x (all_variables y) then
        raise (TypeError((V x),y,[]))
    else
        (try int_unify s (get s x) y with NoMapping -> addSubst s x y)
  | (x,(V y)) ->
    if member y (all_variables x) then
        raise (TypeError(x,(V y),[]))
    else
        (try int_unify s x (get s y) with NoMapping -> addSubst s y x)
  | ((S x),(S y)) ->
    (if x = y then s else raise (TypeError((S x),(S y),[])))
  | ((P (i1,l1)),(P (i2,l2))) ->
    if i1 = i2 && (List.length l1) = (List.length l2) then
        let (m,_) =
            List.fold_left (fun (s,n) -> (fun (p,e) ->
                      ((try int_unify s p e with (TypeError(t1,t2,l)) ->
                      raise (TypeError(t1,t2,n::l))),
                      n + 1)
                 ))
                 (s,0)
                 (pairLists l1 l2)
        in
            m
    else
         raise (TypeError(P(i1,l1),P(i2,l2),[]))
  | ((F (l1,r1)),(F (l2,r2))) ->
      ((try int_unify
          ((try int_unify s l1 l2
           with (TypeError(t1,t2,l)) -> raise (TypeError(t1,t2,0::l))))
          r1 r2
          with (TypeError(t1,t2,l)) -> raise (TypeError(t1,t2,1::l))))
  | (N,N) -> s
  | (t1,tt) -> raise (TypeError(t1,tt,[])) ;;

let unify s t1 t2 =
    let
        res = try int_unify s t1 t2
                  with (TypeError(a,b,l)) -> (print_string ("[" ^ (unparse a) ^ " " ^ (unparse b) ^ "]\n") ;
                                              raise (TypeError(on t1 s,on t2 s,l)))
        (*val _ = print_string "Result:"
        val _ = printSubst res*)
    in
        res
  ;;

let instantiateetype f t =
    let vars = remove_dups (all_variables t) in
    let (m,nf) = List.fold_right
                     (fun a -> (fun (m,nf) ->
                          (addSubst m a (V(Var(nf))),
                           nf + 1
                          )
                     ))
                     vars
                     (id,f)
    in
        (nf,on t m)
    ;;

let applyFunctionetype (F(l,r)) t =
    on l (tmatch r t id) ;;

let getReturnetype x = match x with
  | (F(x,y)) -> y
  | x -> x ;;

let getArgumentetype t n = match t with
  | (F(P(46,l),y)) ->
    if n >= 0 && n < (List.length l) then
        List.nth l n
    else
        raise (TypeError(F(P(intern_t,l),y),F(P(intern_t,l),y),[]))
  | (F(x,y)) -> x
  | t -> raise (TypeError(t,t,[])) ;;

let getArgumentCount t = match t with
  | (F(P(46,l),y)) -> List.length l
  | (F(x,y)) -> 1
  | t -> 0 ;;

let rec getConstructorList tp = match tp with
  | (t,[]) -> [] 
  | (t,((s,l)::b)) -> s::(getConstructorList (t,b)) ;;

let rec isFiniteetype tp = match tp with
  | (t,[]) -> true
  | (t,((s,[])::r)) -> isFiniteetype (t,r)
  | t -> false ;;

exception UndefinedConstructor ;;

let rec getConstructoretype tp s2 = match tp with
  | (t,(s,tl)::r) ->
    if s = s2 then
        if tl = [] then
            t
        else if (List.length tl)=1 then
            F(List.hd tl,t)
        else
            F(P(intern_t,tl),t)
    else
        getConstructoretype (t,r) s2
  | (t,[]) -> raise UndefinedConstructor ;;





