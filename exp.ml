(******************************************************************************
 *
 * REWRITELIB
 *
 * exp.ml
 *
 * This file contains the implementation for the expression type
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

(* require "type-s.sml" ;  *)
(* require "exp-s.sml" ;  *)
(* require "parser.sml" ;  *)
(* require "type.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "list.sml" ;  *)
(* require "basis.__integer" ;  *)
(* require "basis.__substring" ;  *)
(* require "basis.__list" ;  *)

(*open listimpl ;
open TYPEimpl ;
open PARSERimpl ;
open INTERNimpl ;
open L ;*)

(*infix 3 |||| ;*)
(*infix 4 ||| ;*)
(*infix 5 ==> ;*)
(*infix 6 & ;*)

type exp = VAR of int
         | MARKED_VAR of int
         | QUANT of (int * (int * Type.etype) list * exp * exp)
         | APPL of (int * exp list)
         | LET of (exp * Type.etype * exp * exp)
         | CASE of exp * Type.etype * ((exp * exp) list)
         | INDEX of (exp * int * int)
         | HIGHLIGHT of exp
         | NORMAL of exp
         | NUM of int
         | RATIONAL of int * int
         | STRING of string
         | CHAR of char
         | REF of int
         | NOEXP ;;

(*
 * Retrieval functions
 *)
let getName v = match v with
        (APPL (n,e)) -> n
    ;;
let getArgs v = match v with
        (APPL (n,e)) -> e
    ;;
(*
 * Precedence information for parsing/unparsing
 *)

open Parser;;

let prec = [LEFT (SYMBOL "|",1);
            LEFT (SYMBOL "&",2);
            LEFT (SYMBOL ">",3); LEFT (SYMBOL "<",3);
            LEFT (SYMBOL ">=",3); LEFT (SYMBOL "<=",3);
            LEFT (SYMBOL "==",3); LEFT (SYMBOL "!=",3);
            LEFT (SYMBOL "+",4); LEFT (SYMBOL "-",4);
            LEFT (SYMBOL "*",5); LEFT (SYMBOL "*",5);
            RIGHT (SYMBOL "**",6)] ;;

exception NotInfix ;;

let rec getPrecedence s t = match t with
  | [] -> raise NotInfix
  | ((LEFT (SYMBOL t,n))::r) ->
      if (Intern.decode s) = t then n else getPrecedence s r
  | ((RIGHT (SYMBOL t,n))::r) ->
      if (Intern.decode s) = t then n else getPrecedence s r ;;

let rec getLeftPrecedence s t = match t with
  | [] -> raise NotInfix
  | ((LEFT (SYMBOL t,n))::r) ->
      if (Intern.decode s) = t then n else getLeftPrecedence s r
  | ((RIGHT (SYMBOL t,n))::r) ->
      if (Intern.decode s) = t then n + 1 else getLeftPrecedence s r ;;

let rec getRightPrecedence s t = match t with
  | [] -> raise NotInfix
  | ((LEFT (SYMBOL t,n))::r) ->
      if (Intern.decode s) = t then n + 1 else getRightPrecedence s r
  | ((RIGHT (SYMBOL t,n))::r) ->
      if (Intern.decode s) = t then n else getRightPrecedence s r ;;
(*
 * Printing/parsing
 *)
exception BadParse of int ;;

let rec is_nil_cons_list t = match t with
  | (APPL (22,[])) -> true
  | (APPL (21,[a;b])) -> is_nil_cons_list b
  | x -> false
  ;;

let is_cons_list t = match t with
  | (APPL (21,[a;b])) -> is_nil_cons_list b
  | x -> false ;;

let isConstructor s =
    let d = Intern.decode s
    in
        d >= "A" && d <= "ZZZZZ"
    ;;

let rec prExp e = match e with
  | (APPL (1,[a;b;c])) ->
          (prExp a) ^ " { " ^ (prExp c) ^ " } -> " ^ (prExp b)
  | (APPL (1,[a;b])) ->
          (prExp a) ^ " -> " ^ (prExp b)
  | (APPL (2,[a;b;c])) ->
          (prExp a) ^ " { " ^ (prExp c) ^ " } = " ^ (prExp b)
  | (APPL (2,[a;b])) ->
          (prExp a) ^ " = " ^ (prExp b)
  | (APPL (18,[a;b;(APPL (19,[]))])) ->
          "(if " ^ (prExp a) ^ " then " ^ (prExp b) ^ ")"
  | (APPL (18,[a;b;c])) ->
          "(if " ^ (prExp a) ^ " then " ^ (prExp b) ^ " else " ^
          (prExp c) ^ ")"
  | (APPL (s,([l;r]))) ->
    if is_cons_list (APPL (s,[l;r])) then
        prListExp (APPL (s,[l;r]))
    else
        (try
             let space = Intern.intern " " in
             let s1 = if s=Intern.intern "apply" then space else s in
             let left = getLeftPrecedence s1 prec in
             let right = getRightPrecedence s1 prec in
                (if s1=space then
                    (prPrecExp l left) ^ " " ^ (prPrecExp r right)
                else
                    (prPrecExp l left) ^ " " ^ (Intern.decode s) ^ " " ^ (prPrecExp r right))
         with NotInfix ->
            (Intern.decode s) ^ "(" ^ (prExp l) ^ ", " ^ (prExp r) ^  ")")
  | (APPL (s,[])) -> (Intern.decode s) ^ (if isConstructor s then "" else "()")
  | (APPL (s,[x])) -> (Intern.decode s) ^ "(" ^ (prExp x) ^ ")"
  | (APPL (s,l)) ->
    (try
         let left = getLeftPrecedence s prec in
         let right = getRightPrecedence s prec in
             List.fold_left (fun b -> (fun a ->
                       b ^ " " ^ (Intern.decode s) ^ " " ^ (prPrecExp a right)
                  ))
                  (prPrecExp (List.hd l) left)
                  (List.tl l)
    with NotInfix ->  (Intern.decode s) ^ "(" ^ (prExpList l) ^ ")")
  | (VAR s) -> (Intern.decode s)
  | (MARKED_VAR s) -> (Intern.decode s) ^ "'"
  | (INDEX (e,s,i)) -> "index(" ^ (prExp e) ^ ", " ^ (Intern.decode s) ^ ", " ^
                            (string_of_int i) ^ ")"
  | (QUANT (20,t,e,p)) ->
          "{ " ^ (prExp e) ^ " [" ^ (prBound t) ^ "]: " ^ (prExp p) ^
          " }"
  | (QUANT (14,t,e,p)) ->
          "ALL(" ^ (prBound t) ^  ": " ^ (prExp p) ^ ") " ^
          (prExp e)
  | (QUANT (15,t,e,p)) -> "EXISTS(" ^ (prBound t) ^ ") " ^ (prExp e)
  | (QUANT (73,[(v,_)],e,p)) ->
          "(fn " ^ (Intern.decode v) ^ " => " ^ (prExp e) ^ ")"
  | (LET (v,t,e,b)) -> "LET " ^ (prBinding v t e) ^ (prLet b)
  | (CASE (e,t,c)) -> "(CASE " ^ (prExp e) ^ " OF " ^ (prCases c) ^ ")"
  | (HIGHLIGHT e) -> "\027[1m" ^ (prExp e) ^ "\027[0;37m"
  | (NORMAL e) -> "N[" ^ (prExp e) ^ "]"
  | (NUM e) -> (string_of_int e)
  | (RATIONAL (i,j)) -> (string_of_int i) ^ "#" ^ (string_of_int j)
  | (STRING e) -> "\"" ^ e ^ "\""
  | (CHAR e) -> "'" ^ (String.make 1 e) ^ "'"
  | NOEXP -> "NOEXP"
  | (REF x) -> "<<" ^ (string_of_int x) ^ ">>"
  | e -> "<unknown>"
and prListExp (APPL (21,[a;b])) = "[" ^ (prExp a) ^ (prRestExp b)
and prRestExp t = match t with
  | (APPL (22,[])) -> "]"
  | (APPL (21,[a;b])) -> "," ^ (prExp a) ^ (prRestExp b)
and prCases t = match t with
  | [] -> ""
  | ((p,e)::n::r) -> (prExp p) ^ " => " ^ (prExp e) ^ " | " ^
            (prCases (n::r))
  | [(p,e)] -> (prExp p) ^ " => " ^ (prExp e)
and prLet t = match t with
  | (LET (v,t,e,b)) -> "," ^ (prBinding v t e) ^ (prLet b)
  | e -> " IN " ^ (prExp e)
and prBinding v t e =
    if Type.notype=t then (prExp v) ^ "=" ^ (prExp e)
                   else (prExp v) ^ ":" ^ (Type.unparse t) ^ "=" ^
                        (prExp e)
and prBound t = match t with
  | [] -> ""
  | ((a,t)::b::c) ->
    if Type.notype=t then (Intern.decode a) ^ ", " ^ (prBound (b::c))
                   else (Intern.decode a) ^ ":" ^ (Type.unparse t) ^ ", " ^
                        (prBound (b::c))
  | [(a,t)] ->
    if Type.notype=t then (Intern.decode a)
                   else (Intern.decode a) ^ ":" ^ (Type.unparse t)
and prExpList t = match t with
  | [] -> ""
  | ([a]) -> prExp a
  | (a::b::c) -> (prExp a) ^ ", " ^ (prExpList (b::c))
and prPrecExp t n = match t with
  | (APPL (s,(l::rest))) ->
   (try
        let s1=if s=Intern.intern "apply" then Intern.intern " " else s in
        let fixsym = if s1=Intern.intern " " then " " else " " ^ (Intern.decode s1) ^ " " in
            if rest=[] then
                prExp (APPL (s,(l::rest)))
            else (if n > (getPrecedence s1 prec) then
                "(" ^ (prPrecExp l (getLeftPrecedence s1 prec)) ^ " " ^
                (List.fold_left (fun st -> (fun e -> st ^ fixsym ^ (prPrecExp e (getRightPrecedence s1 prec)))) "" rest) ^ ")"
            else
                (prPrecExp l (getLeftPrecedence s prec)) ^ " " ^
                (List.fold_left (fun st -> (fun e -> st ^ fixsym ^ (prPrecExp e (getRightPrecedence s prec)))) "" rest))
    with NotInfix -> prExp (APPL (s,(l::rest))))
  | e -> prExp e ;;

let isUpper s =
    ((int_of_char s) > 64) && ((int_of_char s) < 91) ;;

let rec make_exp_list l = match l with
  | [] -> (APPL (Intern.intern_nil,[]))
  | (a::b) -> (APPL (Intern.intern_cons,[a;make_exp_list b]))
  ;;

let isConstructor s =
    if s=="" then false else isUpper (String.get s 0)

let rec factor l =
            (((((getToken (ID "LET")) & bindings) & (getToken (ID "IN"))) &
                exp) ==> (fun (((a,b),c),d) -> buildLet b d)
            ||| (getToken (ID "NOEXP") ==> (fun (x) -> NOEXP))
            ||| (((((((getToken (ID "ALL")) & (getToken (SPECIAL "("))) &
                (getToken (SPECIAL ":"))) & exp) &
                (getToken (SPECIAL ")"))) & exp) ==>
                (fun (((((a,b),d),e),f),g) ->
                    QUANT (Intern.intern_all,[],g,e)))
            ||| ((((((((getToken (ID "ALL")) & (getToken (SPECIAL "("))) &
                varList) & (getToken (SPECIAL ":"))) & exp) &
                (getToken (SPECIAL ")"))) & exp) ==>
                (fun ((((((a,b),c),d),e),f),g) ->
                    QUANT (Intern.intern_all,c,g,e)))
            ||| (((((getToken (ID "fn")) & (getToken (ID ""))) &
                (getToken (SYMBOL "=>"))) & exp) ==>
                (fun (((a,ID b),c),d) ->
                    QUANT (Intern.intern_lambda,[(Intern.intern b,Type.notype)],d,(APPL (Intern.intern_true,[])))))
            ||| (((((getToken (ID "EXISTS")) & (getToken (SPECIAL "("))) &
                (getToken (SPECIAL ")"))) & exp) ==>
                    (fun (((a,b),d),e) ->
                        QUANT(Intern.intern_exists,[],e,(APPL (Intern.intern_true,[])))))
            ||| (((((((getToken (ID "EXISTS")) & (getToken (SPECIAL "(")))) &
                varList) & (getToken (SPECIAL ")"))) & exp) ==>
                    (fun ((((a,b),c),d),e) ->
                        QUANT(Intern.intern_exists,c,e,(APPL (Intern.intern_true,[])))))
            ||| (((getToken (SPECIAL "[")) & ((exp_list &
                (getToken (SPECIAL "]")))
                    ==> (fun (l,_) -> make_exp_list l)) |||
                ((getToken (SPECIAL "]")) ==> (fun (x) -> make_exp_list []))) ==> (fun (_,l) -> l))
            ||| (((((((((getToken (SPECIAL "{")) & exp) & (getToken (SPECIAL "["))) &
                varList) & (getToken (SPECIAL "]"))) &
                (getToken (SPECIAL ":"))) & exp) & (getToken (SPECIAL "}")))
                    ==>
                    (fun (((((((a,b),c),d),e),f),g),h) ->
                        QUANT (Intern.intern_set,d,b,g)))
            ||| (((((getToken (ID "ALL")) & (getToken (SPECIAL "("))) &
                (getToken (SPECIAL ")"))) & exp) ==>
                (fun (((a,b),d),e) ->
                    QUANT (Intern.intern_all,[],e,(APPL (Intern.intern_true,[])))))
            ||| ((((((getToken (ID "ALL")) & (getToken (SPECIAL "("))) &
                varList) & (getToken (SPECIAL ")"))) & exp) ==>
                (fun ((((a,b),c),d),e) ->
                    QUANT (Intern.intern_all,c,e,(APPL (Intern.intern_true,[])))))
            ||| (((((((getToken (ID "if")) & exp) & (getToken (ID "then"))) &
                exp) & (getToken (ID "else"))) & exp) ==>
                (fun (((((a,b),c),d),e),f) ->
                    APPL (Intern.intern_if,[b;d;f])))
            ||| (((((getToken (ID "if")) & exp) & (getToken (ID "then"))) &
                exp) ==>
                (fun (((a,b),c),d) ->
                    APPL (Intern.intern_if,[b;d;APPL (Intern.intern_undef,[])])))
            ||| ((((getToken (ID "")) & (getToken (SPECIAL "("))) &
                (getToken (SPECIAL ")"))) ==>
                    (fun (((ID a),b),c) -> APPL (Intern.intern a,[])))
            ||| (((((getToken (ID "")) & (getToken (SPECIAL "("))) &
                exp_list) & (getToken (SPECIAL ")"))) ==>
                    (fun ((((ID a),b),c),d) -> APPL (Intern.intern a,c)))
            (*||| (((((getToken (SYMBOL "")) & (getToken (SPECIAL "(")))) &
                exp_list) & (getToken (SPECIAL ")")) ==>
                    (fun ((((SYMBOL a),b),c),d) -> APPL (Intern.intern a;c)))*)
            ||| (getToken (NUMBER 0)    ==>
                (fun (NUMBER n) -> (NUM n)))
            ||| (getToken (RAT (0,0))    ==>
                (fun (RAT (i,j)) -> (RATIONAL (i,j))))
            ||| (getToken (QUOTE "")    ==>
                (fun (QUOTE q) -> (STRING q)))
            ||| (getToken (C_QUOTE '\000')    ==>
                (fun (C_QUOTE q) -> (CHAR q)))
            ||| (getToken (ID "")    ==>
                (fun (ID x) -> if   (isConstructor x)
                              then (APPL (Intern.intern x,[]))
                              else VAR (Intern.intern x)))
            (*||| (((getToken (SPECIAL "(") & exp) & getToken (SPECIAL ")"))
                    ==> (fun ((a,b),c) -> b))*)
             ) l
and varList l = (((((getToken (ID "")) & (getToken (SPECIAL ","))) & varList)
                    ==> (fun ((ID i,x),l) -> ((Intern.intern i,Type.notype)::l)))
             ||| ((getToken (ID "") ==> (fun (ID x) -> [(Intern.intern x,Type.notype)])))) l
and binding l = (((exp & getToken (SYMBOL "=")) & exp) ==>
                    (fun ((a,b),c) -> (a,c))) l
and bindings l = ((((binding & getToken (SPECIAL ",")) & bindings)
                  ==> (fun ((a,b),c) -> (a::c)))
              ||| (binding ==> (fun (x) -> [x]))) l
and conv x ss y = match ss with
  | (Lex.SYMBOL s) -> if s=" " then APPL (Intern.intern "apply",[x;y]) else APPL (Intern.intern s,[x;y])
and buildLet l e = match l with
  | [] -> e
  | ((v,e)::r) -> LET (v,Type.notype,e,buildLet r e)
and exp_list l = (((exp & ((((getToken (SPECIAL ",")) & exp_list) ==>
                      (fun (y,z) -> z))
              ||| (fun (x) -> [(x,[])]))) ==> (fun (x,y) -> x::y))) l
and exp l = precParse prec factor conv l
and rule l = ((exp & (((getToken (SYMBOL "->") & exp)
                  ==> (fun (b,c) ->
                          (APPL (Intern.intern_oriented_rule,[c;APPL (Intern.intern_true,[])]))))
          ||| ((getToken (SYMBOL "=") & exp)
                  ==> (fun (b,c) ->
                          (APPL (Intern.intern_unoriented_rule,[c;APPL (Intern.intern_true,[])]))))
          ||| (((((getToken (SPECIAL "{") & exp) &
              getToken (SPECIAL "}")) & getToken (SYMBOL "->")) & exp)
                  ==> (fun ((((b,c),d),e),f) ->
                          (APPL (Intern.intern_oriented_rule,[f;c]))))
          ||| (((((getToken (SPECIAL "{") & exp) &
              getToken (SPECIAL "}")) & getToken (SYMBOL "=")) & exp)
                  ==> (fun ((((b,c),d),e),f) ->
                          (APPL (Intern.intern_unoriented_rule,[f;c]))))
          ||| (fun (x) -> [(x,NOEXP)]))) ==>
              (fun (a,b) -> match b with NOEXP        -> a
                                   | (APPL (f,l)) -> (APPL (f,a::l)))) l
and rules l = (((getToken (SYMBOL ".") ==> (fun (b) -> [])))
           ||| (((rule & getToken (SYMBOL "@")) & rules) ==> (fun ((a,_),b) -> (a::b)))) l ;;
let parseExp s = let (a,b) = (List.hd (exp (Lex.tokenize s))) in b ;;
let tokenParseExp s = let (a,b) = (List.hd (exp s)) in b ;;
let parseRule s = let (a,b) = (List.hd (rule (Lex.tokenize s))) in b ;;
let tokenParseRule s = let (a,b) = (List.hd (rule s)) in b ;;
let parseRuleList s = let (a,b) = (List.hd (rules (Lex.tokenize s))) in b ;;

exception BadSubterm ;;

let rec getSubterm exp st = match (exp,st) with
  | (exp,[]) -> exp
  | ((QUANT (v,t,e,p)),(0::r)) -> getSubterm e r
  | ((QUANT (v,t,e,p)),(1::r)) -> getSubterm p r
  | ((APPL (s,l)),(n::r)) -> 
    if n < (List.length l) && n >= 0 then
        getSubterm (List.nth l n) r
    else raise BadSubterm
  | ((LET (v,t,e,vl)),(0::r)) -> getSubterm v r
  | ((LET (v,t,e,vl)),(1::r)) -> getSubterm e r
  | ((LET (v,t,e,vl)),(2::r)) -> getSubterm vl r
  | ((CASE (e,t,c)),(0::r)) -> getSubterm e r
  | ((CASE (e,t,c)),(n::r)) ->
    let x = (n-1) / 2 in
    let y = (n-1) mod 2 in
    let qqq = if List.length c < x+1 then raise BadSubterm else () in
    let (p,e) = List.nth c x in
        getSubterm (if y=0 then p else e) r
  | ((INDEX (e,s,i)),(0::r)) -> getSubterm e r
  | (x,y) -> raise BadSubterm ;;

(*fun allSubterms exp =
    let val children =
        case exp
          of (QUANT (v,t,e,p)) =>
              (map (fun (x) -> 0::x) (allSubterms e)) @
              (map (fun (x) -> 1::x) (allSubterms p))
           | (APPL (s,l)) =>
             let val (_,l) =
                 revfold (fun (e,(n,r)) ->
                         (n+1,(map (fun (x) -> n::x) (allSubterms e))@r)
                     ) l (0,[])
             in
                 l
             end
           | (LET (v,t,e,vl)) =>
              (map (fun (x) -> 0::x) (allSubterms e)) @
              (map (fun (x) -> 1::x) (allSubterms e)) @
              (map (fun (x) -> 2::x) (allSubterms vl))
           | (INDEX (e,s,i)) =>
             (map (fun (x) -> 0::x) (allSubterms e))
           | _ => []
    in
        []::children
    end ;*)

let rec revapp l c = match l with
  | [] -> c
  | (a::b) -> revapp b (a::c);;

let subtermCount exp =
    match exp with
       | (QUANT (v,t,e,p)) -> 2
       | (APPL (s,l)) -> List.length l
       | (LET (v,t,e,vl)) -> 3
       | (CASE (e,t,c)) -> 1+((List.length c)*2)
       | (INDEX (e,s,i)) -> 1
       | _ -> 0;;

let rec revAllSubterms prefix front exp =
    let children =
        match exp with
           | (QUANT (v,t,e,p)) ->
              revAllSubterms (0::prefix)
                  (revAllSubterms (1::prefix) front p)
                  e
           | (APPL (s,l)) ->
             let (_,l) =
                 List.fold_right (fun e -> (fun (n,r) ->
                      (n-1,revAllSubterms (n::prefix) r e)
                 )) l ((List.length l)-1,front)
             in
                 l
           | (LET (v,t,e,vl)) ->
              (revAllSubterms (0::prefix)
                  (revAllSubterms (1::prefix)
                      (revAllSubterms (2::prefix) front vl)
                      e)
                  v)
           | (CASE (e,t,c)) ->
              let (_,x) = List.fold_left
                          (fun (n,r) -> (fun (p,e) ->
                              (n+2,
                               revAllSubterms (n::prefix)
                                   (revAllSubterms ((n+1)::prefix) r e) p))
)
                          (1,[]) c
              in
                  (revAllSubterms (0::prefix) x e)
           | (INDEX (e,s,i)) -> revAllSubterms (0::prefix) front e
           | xx -> front
    in
        (List.rev prefix)::children
    ;;

let allSubterms exp = revAllSubterms [] [] exp;;

let rec allSymbols t = match t with
  | (QUANT (v,t,e,p)) -> (allSymbols e)@(allSymbols p)
  | (APPL (s,l)) -> s::(List.fold_right List.append (List.map allSymbols l) [])
  | (LET (v,t,e,vl)) -> (allSymbols v)@(allSymbols e)@(allSymbols vl)
  | (CASE (e,t,c)) -> (allSymbols e) @
    (List.fold_left List.append [] (List.map (fun (x,y) -> (allSymbols x)@(allSymbols y)) c))
  | (INDEX (e,s,i)) -> s::(allSymbols e)
  | (NORMAL e) -> allSymbols e
  | a -> []
  ;;

let rec replacenth l n c = match (l,n) with
  | ((a::b),0) -> (c::b)
  | ((a::b),n) -> a::(replacenth b (n - 1) c) ;;

let rec replaceSubterm exp i exp2 = match (exp,i) with
  | (exp,[]) -> exp2
  | ((QUANT (v,t,e,p)),(0::r)) -> (QUANT (v,t,replaceSubterm e r exp2,p))
  | ((QUANT (v,t,e,p)),(1::r)) -> (QUANT (v,t,e,replaceSubterm p r exp2))
  | ((APPL (s,l)),(n::r)) ->
    if n < (List.length l) && n >= 0 then
        (APPL (s,replacenth l n (replaceSubterm (List.nth l n) r exp2)))
    else raise BadSubterm
  | ((LET (v,t,e,vl)),(0::r)) -> (LET (replaceSubterm v r exp2,t,e,vl))
  | ((LET (v,t,e,vl)),(1::r)) -> (LET (v,t,replaceSubterm e r exp2,vl))
  | ((LET (v,t,e,vl)),(2::r)) -> (LET (v,t,e,replaceSubterm vl r exp2))
  | ((CASE (e,t,c)),(0::r)) -> (CASE ((replaceSubterm e r exp2),t,c))
  | ((CASE (e,t,c)),(n::r)) ->
    let x=(n-1) / 2 in
    let y=(n-1) mod 2 in
    let qqq = if List.length c < x + 1 then raise BadSubterm else () in
    let (a,b) = List.nth c x in
    let (a,b) = if y=1 then (a,replaceSubterm b r exp2)
                           else (replaceSubterm a r exp2,b) in
        (CASE (e,t,replacenth c x (a,b)))
  | ((INDEX (e,s,i)),(0::r)) -> (INDEX (replaceSubterm e r exp2,s,i))
  | x -> raise BadSubterm ;;

let rec remove_normals e = match e with
  | (NORMAL x) -> x
  | (APPL (f,l)) -> (APPL (f, List.map remove_normals l))
  | (LET (v,t,e,p)) -> (LET (v,t,remove_normals e,remove_normals p))
  | (CASE (e,t,c)) -> (CASE (remove_normals e,t,List.map (fun (x,y) -> (x,remove_normals y)) c))
  | (QUANT (v,t,e,p)) -> (QUANT (v,t,remove_normals e,remove_normals p))
  | x -> x
  ;;

