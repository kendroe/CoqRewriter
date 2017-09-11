(******************************************************************************
 *                       
 * REWRITELIB
 *        
 * env.ml  
 *          
 * This module defines the data structure for maintaining the definition,
 * property and inductive hypothesis rules, along with function types and
 * pre-conditions within a specific context.
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

(* require "env-s.sml" ;  *)
(* require "list.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "type.sml" ;  *)
(* require "type.sml" ;  *)
(* require "disc.sml" ;  *)
(* require "context.sml" ;  *)
(* require "expint.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "pp.sml" ;  *)
(* require "exp.sml" ;  *)
(* require "basis.__array" ;  *)
(* require "basis.__integer" ;  *)
(* require "basis.__list" ;  *)
(* require "lm.sml" ;  *)

(* open listimpl ; *)
open Exp ;;
open Intern ;;
(* open TYPEimpl ; *)
(* open DISCimpl ; *)
(* open CONTEXTimpl ; *)
(* open SUBSTimpl ; *)
(* open EXP_INTERNimpl ; *)
(* open PPimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

type parm = S of int
            | E of exp
            | IL of (int list)
            | SL of (int list)
            | I of int
            | WILD
            ;;

type auditItem = RuleStep of (int list) * exp * exp * exp
                   | InternalStep of (int list) * exp * exp
                   | KBStep of (int list) * (exp list) * exp * exp
                   | GoodCondition of (int list) * exp * exp * (auditItem list)
                   | BadCondition of (int list) * exp * exp * (auditItem list)
                   ;;

type constraintType = TrueConstraint
                        | FalseConstraint
                        | UnsolvedConstraint ;;

type envItem = Function of exp * Rtype.etype * exp
                 | FunctionDefinition of int * (exp list)
                 | Attrib of int * (parm list)
                 | Property of exp
                 | TypeDefinition of Rtype.etype * Rtype.typeDef
                 | VarType of int * Rtype.etype
                 | Singular of exp
                 | ImportFile of string
                 | PrecedenceGroup of int * (int list)
                 | PrecedenceFilter of int * (int list)
                 | Expander of int * (exp list list)
                 | FiniteConstructor of int
                 | FiniteType of int
                 ;;

type env = ((envItem list) array) * Disc.disc * Disc.disc * Disc.disc * (int list array) *
           (int list) * (int array) * (int array) *
           (int list array) * int * (exp list) * PrettyPrint.ppenv * (Disc.smallDisc * int list) *
           ((int * (int list)) list) * (exp list) ;;

let getEnvItemList (v,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = v ;;
let setEnvItemList (_,a,b,c,d,e,f,g,h,i,j,k,l,m,n) v = (v,a,b,c,d,e,f,g,h,i,j,k,l,m,n) ;;
let getFunDisc (_,v,_,_,_,_,_,_,_,_,_,_,_,_,_) = v
let setFunDisc (a,_,b,c,d,e,f,g,h,i,j,k,l,m,n) v = (a,v,b,c,d,e,f,g,h,i,j,k,l,m,n)
let getPropDisc (_,_,v,_,_,_,_,_,_,_,_,_,_,_,_) = v
let setPropDisc (a,b,_,c,d,e,f,g,h,i,j,k,l,m,n) v = (a,b,v,c,d,e,f,g,h,i,j,k,l,m,n)
let getViolationDisc (_,_,_,v,_,_,_,_,_,_,_,_,_,_,_) = v
let setViolationDisc (a,b,c,_,d,e,f,g,h,i,j,k,l,m,n) v = (a,b,c,v,d,e,f,g,h,i,j,k,l,m,n)
let getPrecedences (_,_,_,_,v,_,_,_,_,_,_,_,_,_,_) = v
let setPrecedences (a,b,c,d,_,e,f,g,h,i,j,k,l,m,n) v = (a,b,c,d,v,e,f,g,h,i,j,k,l,m,n)
let getNameAways (_,_,_,_,_,v,_,_,_,_,_,_,_,_,_) = v
let setNameAways (a,b,c,d,e,_,f,g,h,i,j,k,l,m,n) v = (a,b,c,d,e,v,f,g,h,i,j,k,l,m,n)
let getEqualGroups (_,_,_,_,_,_,v,_,_,_,_,_,_,_,_) = v
let setEqualGroups (a,b,c,d,e,f,_,g,h,i,j,k,l,m,n) v = (a,b,c,d,e,f,v,g,h,i,j,k,l,m,n)
let getModeList (_,_,_,_,_,_,_,v,_,_,_,_,_,_,_) = v
let setModeList (a,b,c,d,e,f,g,_,h,i,j,k,l,m,n) v = (a,b,c,d,e,f,g,v,h,i,j,k,l,m,n)
let getMinorOrdering (_,_,_,_,_,_,_,_,v,_,_,_,_,_,_) = v
let setMinorOrdering (a,b,c,d,e,f,g,h,_,i,j,k,l,m,n) v = (a,b,c,d,e,f,g,h,v,i,j,k,l,m,n)
let getNextUsableVar (a,b,c,d,e,f,g,h,i,v,j,k,l,m,n) = v
let setNextUsableVar (a,b,c,d,e,f,g,h,i,_,j,k,l,m,n) v = (a,b,c,d,e,f,g,h,i,v,j,k,l,m,n)
let getFailedList (a,b,c,d,e,f,g,h,i,j,v,k,l,m,n) = v
let setFailedList (a,b,c,d,e,f,g,h,i,j,_,k,l,m,n) v = (a,b,c,d,e,f,g,h,i,j,v,k,l,m,n)
let getPrettyPrintenv (a,b,c,d,e,f,g,h,i,j,k,v,l,m,n) = v
let setPrettyPrintenv (a,b,c,d,e,f,g,h,i,j,k,_,l,m,n) v = (a,b,c,d,e,f,g,h,i,j,k,v,l,m,n)
let getContextStuff (a,b,c,d,e,f,g,h,i,j,k,l,v,m,n) = v
let setContextStuff (a,b,c,d,e,f,g,h,i,j,k,l,_,m,n) v = (a,b,c,d,e,f,g,h,i,j,k,l,v,m,n)
let getOverloads (a,b,c,d,e,f,g,h,i,j,k,l,m,v,n) = v
let setOverloads (a,b,c,d,e,f,g,h,i,j,k,l,m,_,n) v = (a,b,c,d,e,f,g,h,i,j,k,l,m,v,n)
let getForbids (a,b,c,d,e,f,g,h,i,j,k,l,m,n,v) = v
let setForbids (a,b,c,d,e,f,g,h,i,j,k,l,m,n,_) v = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,v)

let rec pprec l = match l with
  | [] -> ""
  | ((a,b)::c) -> (" (" ^ a ^ "," ^ b ^ ")" ^ (pprec c))
  ;;

let rec prules l = match l with
  | [] -> ""
  | (a::b) -> ((prExp a) ^ " " ^ (prules b))
  ;;

let rec delete_defs s l = match l with
  | [] -> []
  | (FunctionDefinition(s2,l)::r) ->
    if s=s2 then
        delete_defs s r
    else
        FunctionDefinition(s2,l)::(delete_defs s r)
  | (f::r) -> f::(delete_defs s r)
  ;;

let rec exp_sym e = match e with
  | (APPL (f,l)) -> f
  | (REF x) -> exp_sym (ExpIntern.decode_one_exp (REF x))
  | (QUANT (q,_,_,_)) -> q
  | z -> 0
  ;;

let rec rule_exp_sym e = match e with
  | (APPL (f,[l;r;c])) -> exp_sym l
  | (REF x) -> rule_exp_sym (ExpIntern.decode_one_exp (REF x))
  | x -> exp_sym x
  ;;

let createKey e = match e with
  | (Function (e,_,_)) -> exp_sym e
  | (FunctionDefinition (i,l)) -> i
  | (Attrib (_,((S s)::_))) -> s
  | (Attrib (_,((E e)::_))) -> exp_sym e
  | (Attrib (_,_)) -> Intern.intern_attr
  | (Property (e)) -> rule_exp_sym e
  | (TypeDefinition (t,td)) -> Rtype.getetypeName t
  | (VarType (i,t)) -> i
  | (Singular e) -> rule_exp_sym e
  | (ImportFile s) -> Intern.intern s
  | (PrecedenceGroup (i,il)) -> i
  | (PrecedenceFilter (i,il)) -> i
  | (Expander (i,ell)) -> i
  | (FiniteConstructor i) -> i
  | (FiniteType i) -> i
  ;;

let rec prIlist l = match l with
  | [] -> ""
  | [a] -> string_of_int a
  | (a::b) -> (string_of_int a) ^ "," ^ (prIlist b)

let rec prSlist l = match l with
  | [] -> ""
  | [a] -> "\"" ^ (Intern.decode a) ^ "\""
  | (a::b) -> "\"" ^ (decode a) ^ "\"," ^ (prSlist b) ;;

let rec prParm t = match t with
  | (S s) -> (Intern.decode s)
  | (E e) -> prExp e
  | (IL il) -> "[" ^ prIlist il ^ "]"
  | (SL sl) -> "<" ^ prSlist sl ^ ">"
  | (I i) -> string_of_int i
  | WILD -> "*"
  ;;

let rec prPlist l = match l with
  | [] -> ""
  | [p] -> prParm p
  | (a::b) -> (prParm a) ^ "," ^ (prPlist b) ;;

let prDef d = match d with
  | (Function (e,t,p)) ->
    (prExp e) ^ " type: " ^ (Rtype.pretype t) ^ " pre: " ^ (prExp p)
  | (Attrib (s,pl)) ->
    (Intern.decode s) ^ " " ^ (prPlist pl)
  | (TypeDefinition (t,td)) ->
    (Rtype.unparse t) ^ " = " ^ (Rtype.unparseDef td)
  | (PrecedenceFilter (s,il)) ->
    "Filter " ^ (decode s) ^ ": " ^ (prIlist il)
  | (PrecedenceGroup (s,il)) ->
    "Group " ^ (decode s) ^ ": " ^ (prIlist il)
  | (FunctionDefinition (i,l)) -> "Definition " ^ (decode i) ^
    (prExp (APPL ((intern "r"),l)))
  | (Property (e)) -> "Property " ^ (prExp e)
  | (VarType (i,t)) -> "Var " ^ (Intern.decode i) ^ " " ^ (Rtype.pretype t)
  | (ImportFile s) -> "Import " ^ s
  | _  -> "Unprintable" ;;

let rec upto n = match n with
  | 0 -> []
  | n -> (n-1)::(upto (n-1)) ;;

let rec prDefList l = match l with
  | [] -> ()
  | (a::b) -> (print_string ("[    " ^ (prDef a) ^ "]\n") ; prDefList b) ;;

let rec prDefGroup n l =
    (print_string ("[Group " ^ (Intern.decode n) ^ "(" ^ (string_of_int n) ^ ")]\n") ;
     prDefList l)

let prItemList il =
    List.map (fun (i) -> prDefGroup i (Array.get il i)) (upto (Array.length il)) ;;

let prEqList eq =
    (print_string "Equal list:\n" ;
     List.map (fun (x) -> (print_string ("    " ^ (Intern.decode x) ^ " -> " ^ (Intern.decode (Array.get eq x)) ^ "\n"))) (upto (Array.length eq))) ;;

let rec decodeList l = match l with
  | [] -> ""
  | [a] -> Intern.decode a
  | (a::b) -> (Intern.decode a) ^ " " ^ (decodeList b) ;;

let prPrecList eq =
    (print_string "Prec list:\n" ;
     List.map (fun (x) -> (print_string ("    " ^ (Intern.decode x) ^ " -> " ^ (decodeList (Array.get eq x)) ^ "\n"))) (upto (Array.length eq))) ;;

let precItemGet el n =
    if n < Array.length el then Array.get el n else [] ;;

let max a b = if a < b then b else a ;;

let precItemSet el n v =
    Array.init (max (n+1) (Array.length el))
                   (fun (x) -> if x=n then v else precItemGet el x) ;;

let equalItemGet el n =
    if n < Array.length el then Array.get el n else 0 ;;

let equalItemSet el n v =
    Array.init (max (n+1) (Array.length el))
                   (fun (x) -> if x=n then v else equalItemGet el x)

let envItemGet el n =
    if n < Array.length el then Array.get el n else []

let envItemSet el n v =
    Array.init (max (n+1) (Array.length el))
                   (fun (x) -> if x=n then v else envItemGet el x) ;;

let modeGet el n =
    if n < Array.length el then Array.get el n else 0

let modeSet el n v =
    Array.init (max (n+1) (Array.length el))
                   (fun (x) -> if x=n then v else modeGet el x) ;;

(*
 * Closure checking
 *)
let rec blank_def d = match d with
  | (FunctionDefinition(s,[])::r) -> blank_def r
  | (FunctionDefinition(s,_)::r) -> false
  | (f::r) -> blank_def r
  | _ -> true ;;

let builtins = [Intern.intern_or;Intern.intern_not;Intern.intern_and;Intern.intern_plus;Intern.intern_star;
                Intern.intern_less;Intern.intern_equal;Intern.intern_preceq;Intern.intern_defined;
                Intern.intern_if;Intern.intern_minus] ;;
let builtinsa = [Intern.intern_or;Intern.intern_not;Intern.intern_and] ;;
let builtinsaa = [Intern.intern_equal;Intern.intern_preceq] ;;
let builtinsb = [Intern.intern_plus] ;;
let builtinsc = [Intern.intern_minus] ;;
let builtinsd = [Intern.intern_star] ;;
let builtinse = [Intern.intern_less] ;;
let builtinsf = [Intern.intern_defined;Intern.intern_if;Intern.intern_def;Intern.intern_default] ;;

let is_constructor x = ((Intern.decode x) >= "A" && (Intern.decode x) <= "ZZZZZZZZZ") ;;

let rec is_closed sl el =
    let defs = List.map (fun (x) -> envItemGet el x) sl
    in
        if (List.filter blank_def defs)=[] then
            let defs2 = List.fold_right List.append defs [] in
            let defs3 = List.fold_right List.append (List.map (fun x -> match x with | (FunctionDefinition(s,l)) -> l | _ -> []) defs2) [] in
            let symbols = List.filter
                            (fun (x) -> not(is_constructor x) &&
                                          not(List.mem x builtins) &&
                                          not(x=intern_oriented_rule) &&
                                          not(x=intern_unoriented_rule))
                            (Mylist.remove_dups (List.fold_right List.append (List.map allSymbols defs3) [])) in
            let new_sym = Mylist.difference symbols sl in
                if new_sym=[] then
                    true
                else
                    is_closed (sl@new_sym) el
        else
            false
    ;;

let rec no_type_definition l = match l with
  | [] -> true
  | (TypeDefinition (t,td)::_) -> false
  | (f::r) -> no_type_definition r ;;

let rec is_closed_type sl el =
    let defs = List.map (fun (x) -> envItemGet el x) sl
    in
        if (List.filter no_type_definition defs)=[] then
            let defs2 = List.fold_right List.append (List.map (fun x -> match x with | (TypeDefinition(t,td)) -> [td] | _ -> []) (List.fold_right List.append defs [])) [] in
            let symbols = Mylist.remove_dups (List.fold_right List.append (List.map Rtype.allDefinitionNames defs2) []) in
            let new_sym = Mylist.difference symbols sl in
                if new_sym=[] then
                    true
                else
                    is_closed_type symbols el
        else
            false
    ;;

let subroutine_symbols s l = List.filter
    (fun (x) -> (not(is_constructor x) && not(s=x)
                    && not(List.mem x builtins)
                    && not(x=intern_oriented_rule)
                    && not(x=intern_unoriented_rule)))
    (List.fold_right List.append (List.map allSymbols l) [])
  ;;

let constructor_subtypes s (t,td) =
    Mylist.difference (Rtype.allNames (Rtype.getConstructoretype (t,td) s))
               (Rtype.allNames t)
  ;;

let all_subtypes (t,td) =
    List.fold_right List.append (List.map
        (fun (s) -> constructor_subtypes s (t,td))
        (Rtype.getConstructorList (t,td))) []
  ;;

let match_parm e f = match (e,f) with
  | (WILD,x) -> true
  | ((S(x)),(S(y))) -> (x=y)
  | ((E(x)),(E(y))) -> (x=y)
  | (_,_) -> false
  ;;

let rec is_prefix l p2 = match (l,p2) with
  | ([],_) -> true
  | ((a::b),(x::y)) ->
    (match_parm a x) && (is_prefix b y)
  | (_,_) -> false
  ;;

let rec list_getAttrib a s2 pl2 = match a with
  | ((Attrib (s,pl))::r) ->
    if s=s2 && (is_prefix pl2 pl)
    then pl
    else list_getAttrib r s2 pl2
  | (a::r) -> list_getAttrib r s2 pl2
  | [] -> []
  ;;

let rec all_list_getAttrib a s2 pl2 = match a with
  | ((Attrib (s,pl))::r) ->
    if s=s2 && (is_prefix pl2 pl)
    then pl::(all_list_getAttrib r s2 pl2)
    else all_list_getAttrib r s2 pl2
  | (a::r) -> all_list_getAttrib r s2 pl2
  | [] -> []
  ;;

let rec int_getAttrib l s al = match al with
  | ((S n)::r) ->
    let attr= envItemGet l n
    in
        list_getAttrib attr s ((S n)::r)
  | ((E e)::r) ->
    let attr=envItemGet l (exp_sym e)
    in
        list_getAttrib attr s ((E e)::r)
  | pl ->
    let attr=envItemGet l intern_attr
    in
        list_getAttrib attr s pl
  ;;

let getAttrib env s pl = int_getAttrib (getEnvItemList env) s pl
  ;;

let int_selectAttrib l s i =
    let attr= envItemGet l i
    in
        all_list_getAttrib attr s []
    ;;

let selectAttrib env s i = int_selectAttrib (getEnvItemList env) s i
  ;;

let rec list_allAttrib al s2 pl2 = match al with
  | ((Attrib (s,pl))::r) ->
    if s=s2 && (is_prefix pl2 pl)
    then pl::(list_allAttrib r s2 pl2)
    else list_allAttrib r s2 pl2
  | (a::r) -> list_allAttrib r s2 pl2
  | [] -> []
  ;;

let int_allAttrib el s pl =
    let
        defs = List.fold_right List.append (List.map (fun (x) -> Array.get el x) (upto (Array.length el))) []
    in
        list_allAttrib defs s pl
    ;;

let allAttrib env s pl = int_allAttrib (getEnvItemList env) s pl
  ;;

let isAC env s = (modeGet (getModeList env) s)=3 ;;

let isAorC env s =
    let x = (modeGet (getModeList env) s)
    in
        x>=1 && x<=4
    ;;

let isEQ env s =
    (*(print_string ("Mode of " ^ (Intern.decode s) ^ " is " ^ (string_of_int (modeGet (getModeList env) s)) ^ "\n"));*)
    (modeGet (getModeList env) s)=4 ;;

let isOrder env s = (modeGet (getModeList env) s)=5 ;;

let isImportedi el s = List.mem (ImportFile s) (envItemGet el (intern s)) ;;

let isImported env s = isImportedi (getEnvItemList env) s ;;

let isC env s =
    let x = (modeGet (getModeList env) s)
    in
        x=2 && x=4
    ;;

let isACorC env s =
    let x = (modeGet (getModeList env) s)
    in
        x>=2 && x<=4
    ;;

let isA env s = (modeGet (getModeList env) s)=1 ;;

let getID env s = NOEXP ;;

let rec flatten env e = match e with
  | (APPL (s,l)) ->
    if (isAC env s) then
        (APPL (s, level_out env s l))
    else
        (APPL (s,List.map (flatten env) l))
  | (QUANT (v,t,e,p)) ->
    (QUANT (v,t,flatten env e,flatten env p))
  | (INDEX (e,s,i)) ->
    (INDEX (flatten env e,s,i))
  | x -> x
and level_out env s l = match l with
  | [] -> []
  | ((APPL (s2,l2))::rest) ->
    if s=s2 then
        level_out env s (l2@rest)
    else
        (flatten env (APPL (s2,l2)))::(level_out env s rest)
  | ((NORMAL (APPL (s2,l2)))::rest) ->
    if s=s2 then
        level_out env s ((List.map (fun (x) -> NORMAL(x)) l2)@rest)
    else
        (flatten env (NORMAL (APPL (s2,l2))))::(level_out env s rest)
  | (f::r) ->
        (flatten env f)::(level_out env s r)
  ;;

let rec flatten_top env e = match e with
  | (APPL (s,l)) ->
    if isAC env s then
        (APPL (s,flatten_top_list s l))
    else
        (APPL (s,l))
  | x -> x
and flatten_top_list s e = match e with
  | ((APPL (s2,l))::r) ->
    if s=s2 then
        l@(flatten_top_list s r)
    else
        (APPL (s2,l))::(flatten_top_list s r)
  | ((REF x)::r) ->
    flatten_top_list s ((ExpIntern.decode_exp (REF x))::r)
  | ((NORMAL (REF x))::r) ->
    flatten_top_list s ((NORMAL (ExpIntern.decode_exp (REF x)))::r)
  | ((NORMAL (APPL (s2,l)))::r) ->
    if s=s2 then
        (List.map (fun (x) -> (NORMAL x)) l)@(flatten_top_list s r)
    else
        (NORMAL (APPL (s2,l)))::(flatten_top_list s r)
  | (f::r) -> f::(flatten_top_list s r)
  | [] -> [] ;;

let addItem el item =
    let code = createKey item
    in
        envItemSet el code (item::(envItemGet el code))
    ;;

let emptyModes = modeSet
                    (modeSet
                        (modeSet
                            (modeSet
                                (modeSet
                                    (modeSet
                                        (modeSet
                                            (modeSet
                                                (modeSet
                                                    (Array.make 0 0)
                                                    intern_less 5
                                                )
                                                intern_preceq 5
                                            )
                                            intern_equal 4
                                        )
                                        intern_or 3
                                    )
                                    intern_and 3
                                )
                                intern_rat_plus 3
                            )
                            intern_rat_times 3
                        )
                        intern_nat_plus 3
                    )
                    intern_nat_times 3 ;;

let intGetAC el =
    (int_allAttrib el intern_ac [])@
    (int_allAttrib el intern_a [])@
    (int_allAttrib el intern_c []) ;;

let getAC env = intGetAC (getEnvItemList env) ;;

let reverse_rule (APPL (f,[l;r;c])) = (APPL (Intern.intern_unoriented_rule,[r;l;c])) ;;

let adde gac d e = match e with
  | (APPL (1,l)) -> Disc.add gac d (APPL (Intern.intern_oriented_rule,l))
  | _ -> d
  ;;

let addp gac d e = match e with
  | (APPL (f,l)) -> Disc.add gac (Disc.add gac d (APPL (f,l))) (reverse_rule (APPL (Intern.intern_unoriented_rule,l)))
  | _ -> d
  ;;

let addProperty env e = match e with
  | (APPL (f,[])) -> (env)
  | (APPL (1,ll)) ->
    if List.mem (Property (APPL (Intern.intern_oriented_rule,ll)))
              (envItemGet (getEnvItemList env) (rule_exp_sym (APPL (intern_oriented_rule,ll)))) then
        env
    else
        let gac = (isAorC env) in
        let fl = flatten env (APPL (intern_oriented_rule,ll))
        in
            setFailedList
                (setFunDisc
                    (setPropDisc
                        (setEnvItemList env (addItem (getEnvItemList env) (Property fl)))
                        (Disc.add gac (getPropDisc env) fl))
                    (Disc.add gac (getFunDisc env) fl))
                []
  | (APPL (2,ll)) ->
    if List.mem (Property (APPL (Intern.intern_unoriented_rule,ll)))
              (envItemGet (getEnvItemList env) (rule_exp_sym (APPL (intern_unoriented_rule,ll)))) then
        env
    else
        let gac = (isAorC env) in
        let fl = flatten env (APPL (intern_unoriented_rule,ll))
        in
            setPropDisc
                (setEnvItemList env (addItem (getEnvItemList env) (Property fl)))
                (Disc.add gac (getPropDisc env) fl)
  | x -> (print_string ("bad prop " ^ (prExp x) ^ "\n") ; env) ;;

let rec member_i env a l = match l with
  | [] -> false
  | ((Property f)::r) ->
    a=ExpIntern.intern_exp (isACorC env) f ||
    member_i env a r
  | (f::r) -> member_i env a r ;;

let addViolationRule env r = match r with
  | (APPL (f,[a;b;c])) ->
    let gac = (isAorC env) in
    let fl1 = flatten env (APPL (intern_oriented_rule,[a;b;c])) in
    let fl2 = flatten env (APPL (intern_unoriented_rule,[a;b;c])) in
    let p1 = ExpIntern.intern_exp (isACorC env) fl1 in
    let p2 = ExpIntern.intern_exp (isACorC env) fl2 in
        if member_i env p1 (envItemGet (getEnvItemList env) (rule_exp_sym (APPL (f,[a;b;c])))) ||
           member_i env p2 (envItemGet (getEnvItemList env) (rule_exp_sym (APPL (f,[a;b;c]))))
        then addProperty env fl1
        else env
  | _ -> env ;;

let getContextDisc env = let (a,b) = getContextStuff env in a ;;

let getContextList env = let (a,b) = getContextStuff env in b ;;

let compare a b = if a < b then -1 else if a=b then 0 else 1 ;;

let addContextRules env rl =
    let rl2 = (List.map (fun (REF x) -> x) (List.map (ExpIntern.intern_exp (isACorC env)) rl)) in
    let rl3 = (List.map
                  (fun (REF x) -> x)
                  (List.map
                    (ExpIntern.intern_exp (isACorC env))
                    (List.filter
                     (fun x -> match x with | (APPL (f,l)) -> f=intern_oriented_rule | _ -> false)
                     (List.map ExpIntern.decode_one_exp rl))))
    in
    (*let _ = (List.map (fun x -> print_string ("Adding rule " ^ (prExp (ExpIntern.decode_exp (REF x))) ^ "\n")) rl3) in*)
        setContextStuff env ((Disc.addSmall (getContextDisc env) rl3),
                             List.sort compare (rl3@(getContextList env)))
    ;;

let clearContextRules env =
    setContextStuff env (Disc.newSmall,[]) ;;

let addFailedList env exp =
    setFailedList env (exp::(getFailedList env)) ;;

let checkAC t e d = match (t,e) with
  | (24,[S(s)]) -> Disc.makeAC s d
  | (25,[S(s)]) -> Disc.makeAC s d
  | (26,[S(s)]) -> Disc.makeAC s d
  | _ -> d
  ;;

let markAC t e d = match (t,e) with
  | (24,[S(s)]) -> modeSet d s 3
  | (25,[S(s)]) -> modeSet d s 1
  | (26,[S(s)]) -> modeSet d s 2
  | (32,[S(s);S(e)]) -> modeSet (modeSet d s 5) e 4
  | (30,[S(s);S(e)]) -> modeSet (modeSet d s 5) e 4
  | (31,[S(s);S(e)]) -> modeSet (modeSet d s 5) e 4
  | (27,[S(s);S(e)]) -> modeSet (modeSet d s 5) e 4
  | (28,[S(s)]) -> modeSet d s 4
  | (_,_) -> d
  ;;

let addAttrib env s pl =
    setEnvItemList
        (setFunDisc
            (setPropDisc
                (setViolationDisc
                    (setModeList env (markAC s pl (getModeList env)))
                    (checkAC s pl (getViolationDisc env)))
                (checkAC s pl (getPropDisc env)))
            (checkAC s pl (getFunDisc env)))
        (addItem (getEnvItemList env) (Attrib (s,pl))) ;;

let addImported env s =
    setEnvItemList env (addItem (getEnvItemList env) (ImportFile s)) ;;

let addSingularRule env e =
    setEnvItemList env (addItem (getEnvItemList env) (Singular e)) ;;

let rec list_internal_is_singular l e = match l with
  | [] ->false
  | ((Singular x)::r) ->
    if x=e then
        true
    else
        list_internal_is_singular r e
  | (a::b) -> list_internal_is_singular b e
  ;;

let internal_is_singular el e = list_internal_is_singular (envItemGet el (rule_exp_sym e)) e ;;

let isSingularRule env e = internal_is_singular (getEnvItemList env) e ;;

let rec listDeleteRules s l = match l with
  | [] -> []
  | (FunctionDefinition(t,r)::rest) ->
    if s = t then rest else FunctionDefinition(t,r)::listDeleteRules s rest
  | (a::rest) -> a::(listDeleteRules s rest) ;;

let deleteRules s el = envItemSet el s (listDeleteRules s (envItemGet el s)) ;;

let rec listGetRules1 s l = match l with
  | [] -> []
  | (FunctionDefinition(t,r)::rest) ->
    if s = t then r else listGetRules1 s rest
  | (a::rest) -> listGetRules1 s rest ;;

let getRules1 s el =
    listGetRules1 s (envItemGet el s) ;;

let newDefinition env (s,rules) =
    let gac = (isAorC env)
    in
        setEnvItemList
           (setFunDisc
               (setPropDisc
                   env
                   (List.fold_right (fun r -> (fun d -> Disc.add gac d r)) rules (getPropDisc env)))
               (List.fold_right (fun r -> (fun d -> adde gac d r)) rules (getFunDisc env)))
           (addItem (deleteRules s (getEnvItemList env))
                    (FunctionDefinition (s,rules)))
    ;;

let addVarType env (v,t) =
    setEnvItemList env (addItem (getEnvItemList env) (VarType (v,t))) ;;

let stripVarType el v =
    envItemSet el v (List.filter
                        (fun x -> match x with | (VarType (x,_)) -> not(x=v) | _ -> true)
                        (envItemGet el v))

let stripVarTypes env vs =
    setEnvItemList env (List.fold_left stripVarType vs (getEnvItemList env)) ;;

let getRules = getFunDisc ;;

let getRulesDoubled = getPropDisc ;;

exception UndefinedSymbol of int ;;

let rec listGetTypePre fl s2 = match fl with
  | ((Function (APPL(s,fp),t,p))::r) ->
    if s = s2 then (t,(APPL(s,fp)),p) else listGetTypePre r s2
  | (_::r) -> listGetTypePre r s2
  | [] -> (print_string ("listGetTypePre  " ^ (Intern.decode s2) ^ "\n"));(raise (UndefinedSymbol(s2))) ;;

let getTypePre el s = listGetTypePre (envItemGet el s) s ;;

let getFunctionType env s =
    let (t,_,_) = getTypePre (getEnvItemList env) s in t ;;

let getFunctionPrecondition env s =
    let (t,e,c) = getTypePre (getEnvItemList env) s in (e,c) ;;

let rec listGetDef l s2 = match l with
  | ((FunctionDefinition (s,e))::r) ->
    if s = s2 then e else listGetDef r s
  | (_::r) -> listGetDef r s2 ;;

let getDef el s = listGetDef (envItemGet el s) s ;;

let getFunctionDefinition env s =
    getDef (getEnvItemList env) s ;;

let rec listGetTDef l s = match l with
  | ((TypeDefinition (t,tdef))::r) ->
    if s = Rtype.getetypeName t then (t,tdef) else listGetTDef r s
  | (_::r) -> listGetTDef r s
  | [] -> print_string "listGetTDef";raise (UndefinedSymbol s) ;;

let getTDef el s = listGetTDef (envItemGet el s) s ;;

let getTypeDefinition env s =
    getTDef (getEnvItemList env) s ;;

let rec listGetVType l s2 = match l with
  | ((VarType (s,t))::r) ->
    if s = s2 then t else listGetVType r s
  | (_::r) -> listGetVType r s2
  | [] -> print_string "listGetVType\n";raise (UndefinedSymbol(s2)) ;;

let getVType el s = listGetVType (envItemGet el s) s ;;

let getVarType env s =
    getVType (getEnvItemList env) s ;;

exception CircularPrecedence ;;

let addP (s1,s2) pr =
   if (is_constructor s2) && (not (is_constructor s1)) then
        raise CircularPrecedence
   else if (List.mem s1 (precItemGet pr s2)) || s1=s2 then
        raise CircularPrecedence
   else if List.mem s2 (precItemGet pr s1) then
        pr
   else
       let bigger = precItemGet pr s2 in
       let smaller = List.filter
                     (fun (s) -> List.mem s1 (precItemGet pr s))
                     (upto (Array.length pr)) in
       let pairs = List.combine (s1::smaller) (s2::bigger) in
       let rec add (s1,s2) pr =
                   precItemSet pr s1 (
                   let g = precItemGet pr s1
                   in
                       if List.mem s2 g then g else s2::g)
       in
           List.fold_right add pairs pr
       ;;

let rec ges a s =
    let n = equalItemGet a s
    in
        if s=n || n=0 then s else ges a n
    ;;

let addIntPrecedence env (s2,s1) =
    setPrecedences env (addP (ges (getEqualGroups env) s1,ges (getEqualGroups env) s2) (getPrecedences env)) ;;

let rec mappr s el l = match l with
  | [] -> []
  | ((a,b)::r) ->
    ((if List.mem a s then el else a),(if List.mem b s then el else b))::
    (mappr s el r) ;;

let addIntEqualPrecedence env (s1,s2) =
    let eq = getEqualGroups env in
    let v1 = ges eq s1 in
    let v2 = ges eq s2 in
    let eq2 = equalItemSet eq v2 v1 in
        setEqualGroups env eq2
    ;;

let addTypePrecedence env t1 t2 =
    List.fold_right (fun s1 -> (fun env ->
            List.fold_right
                (fun s2 -> (fun env ->
                    addIntPrecedence env (s2,s1)
                ))
                (Rtype.getConstructorList t2)
                env
        ))
        (Rtype.getConstructorList t1) env
  ;;

let hasSmallerPrecedence env s1 s2 =
    if (is_constructor s1) && (not (is_constructor s2)) then
        true
    else if (List.mem s1 builtins) && (not (is_constructor s2))
            && (not (List.mem s2 builtins)) then
        true
    else if (List.mem s2 builtins) && (is_constructor s1) then
        true
    else if (List.mem s1 builtinsa) && (List.mem s2 (builtinsaa@builtinsb@builtinsc@builtinsd@builtinse@builtinsf)) then
        true
    else if (List.mem s1 builtinsaa) && (List.mem s2 (builtinsb@builtinsc@builtinsd@builtinse@builtinsf)) then
        true
    else if (List.mem s1 builtinsb) && (List.mem s2 (builtinsc@builtinsd@builtinse@builtinsf)) then
        true
    else if (List.mem s1 builtinsc) && (List.mem s2 (builtinsd@builtinse@builtinsf)) then
        true
    else if (List.mem s1 builtinsd) && (List.mem s2 (builtinse@builtinsf)) then
        true
    else if (List.mem s1 builtinse) && (List.mem s2 builtinsf) then
        true
    else
        List.mem (ges (getEqualGroups env) s1)
               (precItemGet (getPrecedences env)
                   (ges (getEqualGroups env) s2))

let hasEqualPrecedence env s1 s2 =
    (List.mem s1 builtinsf && List.mem s2 builtinsf) ||
    (List.mem s1 builtinse && List.mem s2 builtinse) ||
    (List.mem s1 builtinsd && List.mem s2 builtinsd) ||
    (List.mem s1 builtinsc && List.mem s2 builtinsc) ||
    (List.mem s1 builtinsb && List.mem s2 builtinsb) ||
    (List.mem s1 builtinsa && List.mem s2 builtinsa) ||
    (List.mem s1 builtinsaa && List.mem s2 builtinsaa) ||
    (ges (getEqualGroups env) s1)=(ges (getEqualGroups env) s2)

let builtin_types = [intern "Natural"; intern "Char"; intern "String"] ;;

let isSmallerType env s1 s2 =
    if List.mem s1 builtin_types then
       (if List.mem s2 builtin_types then
            false
        else
            true)
    else
        if List.mem s2 builtin_types then
            false
        else
            try let t1 = getTypeDefinition env s1 in
            let t2 = getTypeDefinition env s2 in
            let c1 = Rtype.getConstructorList t1 in
            let c2 = Rtype.getConstructorList t2 in
                if c1=[] || c2=[] then false
                else hasSmallerPrecedence env (List.hd c1) (List.hd c2)
            with UndefinedSymbol(s) -> false ;;

let rec larger_than_all env s l = match l with
  | [] -> true
  | (a::b) ->
    (isSmallerType env a s) && (larger_than_all env s b)
  ;;

let rec has_largest env l x = match l with
  | [] -> false
  | (a::b) ->
    (larger_than_all env a x) || (has_largest env b x)
  ;;

let smaller_constructor env (t,td) s1 s2 =
    if s1=s2 then
        false
    else
        let t1 = Rtype.getConstructoretype (t,td) s1 in
        let t2 = Rtype.getConstructoretype (t,td) s2 in
        let n = Rtype.getetypeName t
        in
            if Rtype.getArgumentCount t1=0 &&
               Rtype.getArgumentCount t2>0 then
                true
            else
                has_largest env (Mylist.delete n (Rtype.allNames t2))
                                (Mylist.delete n (Rtype.allNames t1))
  ;;

let rec addPrecedenceList env a l = match l with
  | [] -> env
  | (b::c) ->
    addPrecedenceList (addIntPrecedence env (a,b)) a c
  ;;

let rec addPrecedenceMatrix env l x = match l with
  | [] -> env
  | (a::b) ->
    addPrecedenceMatrix (addPrecedenceList env a x) b x
  ;;

let addTypePrecedences env (t,tdef) =
    let n = Rtype.getetypeName t in
    let l= getEnvItemList env in
    let st = List.filter 
             (fun (x) -> is_closed_type [x] l)
             (Mylist.delete n (Rtype.allDefinitionNames tdef)) in
    let cl = Rtype.getConstructorList (t,tdef) in
    let env2 = List.fold_right
            (fun tn -> (fun env ->
                (try addPrecedenceMatrix env
                    (Rtype.getConstructorList (getTypeDefinition env tn)) cl
                with UndefinedSymbol(s) -> env)
            )) st env in
    let env3 =
            List.fold_right (fun s1 -> (fun env ->
                     List.fold_right (fun s2 -> (fun env ->
                         if smaller_constructor env (t,tdef) s1 s2 then
                              addIntPrecedence env (s1,s2)
                         else
                             env
                     )) cl env
                 )) cl env2
    in
        env3
    ;;

let addTypeDefinition env (t,def) =
    let env1 =
        addTypePrecedences
            (setEnvItemList env
                (envItemSet
                    (getEnvItemList env)
                    (Rtype.getetypeName t)
                    (TypeDefinition(t,def)::(envItemGet (getEnvItemList env) (Rtype.getetypeName t)))))
            (t,def)
    in
        if Rtype.isFiniteetype (t,def) then
            let r1 = (List.fold_left (fun env -> (fun c ->
                             (setEnvItemList env
                                 (addItem
                                     (getEnvItemList env)
                                     (FiniteConstructor c)))
                             )) env1 (Rtype.getConstructorList (t,def)))
            in
                (setEnvItemList r1
                    (addItem
                        (getEnvItemList r1)
                        (FiniteType (Rtype.getetypeName t))
                    )
                )
        else env1
    ;;

let isFiniteConstructor env c =
    not((List.filter
          (fun x -> match x with | (FiniteConstructor _) -> true | _ -> false)
          (envItemGet (getEnvItemList env) c))=[])

let isFiniteType env c =
    not((List.filter
          (fun x -> match x with | (FiniteType _) -> true | _ -> false)
          (envItemGet (getEnvItemList env) c))=[]) ;;

let isConstructor x =
    let f = String.get (Intern.decode x) 0 in
        (f >= 'A' && f <= 'Z')
    ;;

let rec getCType l x = match l with
  | [] -> print_string "getCType" ; raise (UndefinedSymbol(x))
  | ((TypeDefinition (t,td))::r) ->
    (try (Rtype.getConstructoretype (t,td) x)
    with Rtype.UndefinedConstructor -> getCType r x)
  | (_::r) -> getCType r x ;;

let eGetCType el x = getCType (List.fold_right List.append (List.map (fun (n) -> envItemGet el n) (upto (Array.length el))) []) x ;;

let getConstType env x =
    eGetCType (getEnvItemList env) x ;;

let getOverloadChoices env s =
    let overloads = getOverloads env in
        let rec f l = match l with
          | [] -> []
          | ((ss,l)::r) -> if s=ss then l else f r
    in
        f overloads
    ;;

let getType env x =
    if getOverloadChoices env x=[] then
       (if isConstructor x then
            getConstType env x
        else
            getFunctionType env x)
    else
        Rtype.mkTfun (Rtype.mkVar (Rtype.newVar ())) (Rtype.mkProduct intern_t
                [Rtype.mkVar (Rtype.newVar ());Rtype.mkVar (Rtype.newVar ());Rtype.mkVar (Rtype.newVar ());Rtype.mkVar (Rtype.newVar ()); Rtype.mkVar (Rtype.newVar ());
                 Rtype.mkVar (Rtype.newVar ());Rtype.mkVar (Rtype.newVar ());Rtype.mkVar (Rtype.newVar ());Rtype.mkVar (Rtype.newVar ()); Rtype.mkVar (Rtype.newVar ())]) ;;

let getArity env x = Rtype.getArgumentCount (getType env x) ;;

let getAllConstructors env =
    (List.map
        (fun (TypeDefinition(x,y)) -> Rtype.getConstructorList (x,y))
        (List.filter
            (fun (x) -> (match x with | (TypeDefinition(a,b) ) -> true
                                      | _                  -> false))
            (List.fold_right List.append
                (List.fold_right List.append
                    (List.map (fun (x) -> envItemGet (getEnvItemList env) x) (upto (Array.length (getEnvItemList env)))) []) []))) ;;

let rec getCType2 l x = match l with
  | [] -> print_string "getCType2\n";raise (UndefinedSymbol(x))
  | ((TypeDefinition (t,td))::r) ->
    if List.mem x (Rtype.getConstructorList (t,td)) then
        (t,td)
    else
        getCType2 r x
  | (_::r) -> getCType2 r x ;;

let eGetCType2 el x = getCType2 (List.fold_right List.append (List.map (fun (n) -> envItemGet el n) (upto (Array.length el))) []) x ;;

let getConstType2 env x =
    eGetCType2 (getEnvItemList env) x ;;

let rec zero_argument_constructors l t = match l with
  | [] -> 0
  | (a::b) ->
    (zero_argument_constructors b t)+
    (if (Rtype.getArgumentCount (Rtype.getConstructoretype t a))=0 then
         1
     else
         0)
  ;;

let smallestSymbolOfType env s =
    let (t,tdef) = getConstType2 env s in
    let cl = Rtype.getConstructorList (t,tdef)
    in
        ((Rtype.getArgumentCount (Rtype.getConstructoretype (t,tdef) s))=0)
        &&
        ((zero_argument_constructors cl (t,tdef))=1)
    ;;

let addNameAways env na2 = setNameAways env ((getNameAways env)@na2) ;;

let rec strip l = match l with
  | [] -> []
  | ((a,b)::r) -> a::(strip r) ;;

let rec unstrip l = match l with
  | [] -> []
  | (a::b) -> (a,Rtype.notype)::(unstrip b) ;;

let rec map_away_vars u l = match l with
  | [] -> []
  | (a::b) ->
    (let (VAR x) = try Rsubst.apply u a with Rsubst.NoUnifier -> (VAR a) in
     x)::(map_away_vars u b) ;;

let getAllRules env =
    let el=getEnvItemList env
    in
        (List.map
            (fun (Property x) -> x)
            (List.filter
                (fun (x) -> (match x with | (Property x) -> true | _ -> false))
                (List.fold_right List.append
                    (List.map (fun (x) -> envItemGet el x) (upto (Array.length el))) []))) ;;

(*fun print_string_env env = ("Prec: " ^ (pprec (getPrecedences env)) ^ " Rules: " ^ (prules (getAllRules env))) ;*)
let print_env env = ""

let rec name_away_bound_vars e string_list =
    let (l,u,e) = aug_name_away e string_list Rsubst.empty (Rcontext.getFreeVars e)
    in
        (u,e)
and
    aug_name_away e sl u away = match e with
  | (VAR x) ->
        if (List.mem x (Rsubst.dom u)) then
            (sl,u,(Rsubst.apply u x))
        else if (List.mem x sl) then
            let x1=Rcontext.name_away_from_list (sl@away) x in
                ((x1::sl),(Rsubst.addPair u x (VAR x1)),(VAR x1))
        else
            (sl,u,(VAR x))
  | (MARKED_VAR x) -> (sl,u,MARKED_VAR x)
  | (APPL (s,l)) ->
    let (l,u,ll) =
        List.fold_right (fun e -> (fun (sl,u,r) ->
              let (sl,u,f) = aug_name_away e sl u away
              in
                  (sl,u,(f::r))
             )) l (sl,u,[])
    in
        (l,u,(APPL (s,ll)))
  | (QUANT (n,v,e,p)) ->
    let vars = strip v in
    let free_binds = Rsubst.select_subs u vars in
    let away2 = (away@vars) in
    let (sl1,u1,e1) = aug_name_away e sl (Rsubst.remove_subs u vars) away2 in
    let (sl2,u2,p1) = aug_name_away p sl1 u1 away2 in
    let v2 = unstrip (map_away_vars u2 vars) in
        (sl2,(Rsubst.merge_subs free_binds (Rsubst.remove_subs u2 vars)),
         (QUANT (n,v2,e1,p1)))
  | (LET (v,t,b,e)) ->
    let vars = Rcontext.getFreeVars v in
    let (sl1,u1,b1) = aug_name_away b sl u away in
    let (sl2,u2,v1) = aug_name_away v sl1 (Rsubst.remove_subs u1 vars)
                      (away@vars) in
    let (sl3,u3,e1) = aug_name_away e sl2 u2
                      (away@vars)
    in
        (sl3,(Rsubst.merge_subs (Rsubst.select_subs u1 vars) (Rsubst.remove_subs u3 vars)),
         (LET (v1,t,b1,e1)))
  | (CASE (e,t,c)) ->
    let (sl1,u1,e1) = aug_name_away e sl u away in
    let (sl2,u2,c1) = List.fold_left
                          (fun (sl,u,l) -> (fun (p,e) ->
                              let vars = Rcontext.getFreeVars p in
                              let (sl,u,e2) = aug_name_away e sl (Rsubst.remove_subs u vars) away
                              in
                                  (sl,u,(p,e2)::l)
                          ))
                          (sl1,u1,[]) c
    in
        (sl2,u2,(CASE (e1,t,c1)))
  | (NORMAL x) ->
    let (sl1,u1,x1) = aug_name_away x sl u away in
        (sl1,u1,(NORMAL x1))
  | x ->
    (sl,u,x)
  ;;

let addGroup env s sl =
    setEnvItemList env (addItem (getEnvItemList env) (PrecedenceGroup (s,sl))) ;;

let addFilter env s il =
    setEnvItemList env (addItem (getEnvItemList env) (PrecedenceFilter (s,il))) ;;

let rec gg l s = match l with
  | [] -> []
  | ((PrecedenceGroup (s2,sl))::l) ->
    if s=s2 then sl else gg l s
  | (_::r) -> gg r s

let getGroup env s = gg (envItemGet (getEnvItemList env) s) s ;;

let rec gf l s = match l with
  | [] -> []
  | ((PrecedenceFilter (s2,sl))::l) ->
    if s=s2 then sl else gf l s
  | (_::r) -> gf r s ;;

let getFilter env s = gf (envItemGet (getEnvItemList env) s) s ;;

let filterList env s el =
    let x = getFilter env s
    in
        if x = [] then
            el
        else
            List.map (fun (x) -> List.nth el x) x
    ;;

let addPrecedence env (s1,s2) =
    let ll1=getGroup env s1 in
    let l1 = if ll1=[] then [s1] else ll1 in
    let ll2=getGroup env s2 in
    let l2 = if ll2=[] then [s2] else ll2 in
    let pairs = List.combine l1 l2
    in
        List.fold_right (fun a -> (fun e -> addIntPrecedence e a)) pairs env
    ;;

let addEqualPrecedence env (s1,s2) =
    let ll1=getGroup env s1 in
    let l1 = if ll1=[] then [s1] else ll1 in
    let ll2=getGroup env s2 in
    let l2 = if ll2=[] then [s2] else ll2 in
    let pairs = List.combine l1 l2
    in
        List.fold_right (fun a -> (fun e -> addIntEqualPrecedence e a)) pairs env
    ;;

let addFunction env (APPL(n,fp),t,c,rules) rules2 =
    let gac = (isAorC env) in
    let ss = Mylist.remove_dups (subroutine_symbols n rules) in
    let lss = List.filter (fun (s) -> is_closed [s] (getEnvItemList env)) ss in
    let ess = Mylist.difference ss lss in
    let env2 =
            List.fold_right
                (fun s -> (fun e -> addPrecedence e (s,n)))
                lss
                (List.fold_right
                    (fun s -> (fun e -> addEqualPrecedence e (s,n))) ess
                    (setEnvItemList env
                        (addItem
                            (addItem
                                (getEnvItemList env)
                                (Function (APPL(n,fp),t,c)))
                            (FunctionDefinition (n,List.map (flatten env) rules))))) in
    (*let _ = print_string "[Function rules]\n" in
    let _ = List.map (fun (x) -> (print_string ("[    " ^ (prExp x) ^ "]\n"))) rules in
    let _ = print_string "[Function rules2]\n" in
    let _ = List.map (fun (x) -> (print_string ("[    " ^ (prExp x) ^ "]\n"))) rules2 in*)
    let res =
            setFunDisc
               (setPropDisc
                   env2
                   (List.fold_right (fun r -> (fun d -> Disc.add gac d r)) (List.map (flatten env) rules2) (getPropDisc env2)))
               (List.fold_right (fun r -> (fun d -> adde gac d r)) (List.map (flatten env) rules) (getFunDisc env2)) in
                let newItems =
            List.fold_right (fun p -> (fun e -> addItem e (Property p))) rules2 (getEnvItemList res) in
        setEnvItemList res newItems
    ;;

let allEnvItems env = (List.fold_right List.append (List.map (fun (x) -> (envItemGet (getEnvItemList env) x)) (upto (Array.length (getEnvItemList env)))) []) ;;

let getFunctions env =
       List.map
           prDef
           (List.filter
               (fun (x) -> match x with | (Function (e,t,p)) -> true | _ -> false)
               (allEnvItems env)) ;;

let getFunctionDetail env n =
    let d = List.fold_right List.append (List.map (fun (x) -> envItemGet (getEnvItemList env) x) (upto (Array.length (getEnvItemList env)))) [] in
    let (Function (APPL (f,l),t,p)) = List.nth (List.filter (fun (x) -> match x with(Function (e,t,p)) -> true | _ -> false) d) n in
    let rules = List.map (fun (x) -> ("    " ^ (prExp x))) (listGetRules1 f d)
    in
        (prDef (Function (APPL (f,l),t,p)))::rules
    ;;

let getTypes env =
         List.map prDef (List.filter
             (fun (x) -> match x with (TypeDefinition (t,td))-> true | _ -> false)
             (allEnvItems env)) ;;

let getAttributes env =
         List.map prDef (List.filter
             (fun (x) -> match x with (Attrib (s,pl))-> true | _ -> false)
             (allEnvItems env)) ;;

let rec expander_list l = match l with
  | [] -> ""
  | [a] -> prExp a
  | (a::b) -> "< " ^ (prExp a) ^ " " ^ (expander_list b) ^ " >" ;;

let rec expander_lists l =  match l with
  | [] -> ""
  | [a] -> "<" ^ (expander_list a) ^ ">"
  | (a::b) -> "<" ^ (expander_list a) ^ "> " ^ (expander_lists b) ;;

let rec get_sp l = match l with
  | [] -> []
  | (PrecedenceFilter(s,il)::r) ->
    ("Filter " ^ (Intern.decode s) ^ (prIlist il))::(get_sp r)
  | (PrecedenceGroup (s,sl)::r) ->
    ("Group " ^ (decode s) ^ " => " ^ (prSlist sl))::(get_sp r)
  | (Expander (s,ell)::r) ->
    ("Expanders " ^ (decode s) ^ " (" ^ (expander_lists ell) ^ ")")::(get_sp r)
  | (f::r) -> get_sp r ;;

let getPrecedenceInfo env =
    (get_sp (allEnvItems env))@(List.map (fun (x) -> ("Force " ^ (prExp x))) (Disc.allExps (getViolationDisc env)))

let getEqualSymbols env s =
    let eg = getEqualGroups env in
    let base = ges eg s in
    let symbols = (List.filter
                      (fun (x) -> equalItemGet eg x=base)
                      (upto (Array.length eg)))
    in
        symbols
    ;;

let getAllSymbols env =
    List.fold_right List.append
          (List.map (fun (x) -> match x with
                             | (Function (APPL (f,l),_,_)) -> [f]
                             | (TypeDefinition (t,td)) -> Rtype.getConstructorList (t,td)
                             | _ -> []
               ) (allEnvItems env)) []

let  getSmallerSymbols env s =
    List.filter
        (fun (x) -> hasSmallerPrecedence env x s)
        (getAllSymbols env) ;;

let getGreaterSymbols env s =
    List.filter
        (fun (x) -> hasSmallerPrecedence env s x)
        (getAllSymbols env) ;;

let rec getE l s = match l with
  | [] -> []
  | (Expander (s2,ll)::r) ->
    if s=s2 then ll else getE r s
  | (_::r) -> getE r s ;;

let getExpanders env s =
    getE (envItemGet (getEnvItemList env) s) s ;;

let rec merge a b = match (a,b) with
  | ([],b) -> b
  | (a,[]) -> a
  | ((a::b),(c::d)) ->
    (Mylist.remove_dups (a@c))::(merge b d)

let rec addE e s ll = match e with
  | [] -> [(Expander (s,ll))]
  | (Expander (s1,l1)::r) ->
    if s1=s then
        (Expander (s1,merge l1 ll))::r
    else
        (Expander (s1,l1))::(addE r s ll)
  | (x::r) -> x::(addE r s ll) ;;

let addExpanders env s ll =
    setEnvItemList env
        (envItemSet (getEnvItemList env) s
            (addE (envItemGet (getEnvItemList env) s) s ll)) ;;

let addMP pr s1 s2 =
    let bigger = precItemGet pr s2 in
    let smaller = List.filter
                      (fun (s) -> List.mem s1 (precItemGet pr s))
                      (upto (Array.length pr)) in
    let pairs = List.combine (s1::smaller) (s2::bigger) in
    let add (s1,s2) pr =
        precItemSet pr s1 (s2::(precItemGet pr s1))
    in
        List.fold_right add pairs pr
    ;;

let addMinorPrecedence env s1 s2 =
    setMinorOrdering env (addMP (getMinorOrdering env) s1 s2)

let hasSmallerMinorPrecedence env s1 s2 =
    List.mem s2 (precItemGet (getMinorOrdering env) s1) ;;

let addDirective env d =
    setPrettyPrintenv env (PrettyPrint.addDirective (getPrettyPrintenv env) d) ;;

let addParseDirectives env s =
    setPrettyPrintenv env (PrettyPrint.addParseDirectives (getPrettyPrintenv env) s) ;;

let ppExp env e i =
    PrettyPrint.ppExp (getPrettyPrintenv env) e i ;;

let ppOneLine env e =
    PrettyPrint.ppOneLine (getPrettyPrintenv env) e ;;

let ppParse env s =
    PrettyPrint.ppParse (getPrettyPrintenv env) s ;;

let ppParsePos env f s =
    PrettyPrint.ppParsePos (getPrettyPrintenv env) f s ;;

let rec remove_overload s l = match l with
  | [] -> []
  | ((f,fl)::r) ->
    if s=f then r else (f,fl)::(remove_overload s r) ;;

let addOverload env s sl =
    let overloads = getOverloads env
    in
        setOverloads env ((s,sl)::(remove_overload s overloads))
    ;;

let getConversions env = getOverloadChoices env (-1) ;;

let addConversion env c =
    let x = getConversions env
    in
        addOverload env (-1) (c::x)
    ;;

let l1 =
     [TypeDefinition (Rtype.parse "Bool",
                      Rtype.parseDef "True|False");
      FiniteType(Intern.intern_bool);
      FiniteConstructor(Intern.intern_true);
      FiniteConstructor(Intern.intern_false);
      TypeDefinition (Rtype.parse "Unit",
                      Rtype.parseDef "Identity|Trivial");
      FiniteType(Intern.intern_unit);
      FiniteConstructor(Intern.intern_identity);
      FiniteConstructor(Intern.intern_trivial);
      TypeDefinition (Rtype.parse "Natural",Rtype.emptyDef);
      TypeDefinition (Rtype.parse "Rational",Rtype.emptyDef);
      TypeDefinition (Rtype.parse "String",Rtype.emptyDef);
      TypeDefinition (Rtype.parse "Char",Rtype.emptyDef);
      Function (Exp.parseExp "defined(a)",Rtype.parse "'a -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "a&b", Rtype.parse "Bool * Bool -> Bool",parseExp "True");
      Function (Exp.parseExp "not(a)", Rtype.parse "Bool -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "a|b", Rtype.parse "Bool * Bool -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "Default(a)", Rtype.parse "Bool -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "Def(a)", Rtype.parse "Bool -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "a==b", Rtype.parse "'a * 'a -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "natrat(a)", Rtype.parse "Natural -> Rational",Exp.parseExp "True");
      Function (Exp.parseExp "ratnat(a)", Rtype.parse "Rational -> Natural",Exp.parseExp "True");
      Function (Exp.parseExp "nless(a,b)", Rtype.parse "Natural * Natural -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "rless(a,b)", Rtype.parse "Rational * Rational -> Bool",Exp.parseExp "True");
      Function (Exp.parseExp "nplus(a,b)", Rtype.parse "Natural * Natural -> Natural",Exp.parseExp "True");
      Function (Exp.parseExp "rplus(a,b)", Rtype.parse "Rational * Rational -> Rational",Exp.parseExp "True");
      Function (Exp.parseExp "ntimes(a,b)", Rtype.parse "Natural * Natural -> Natural",Exp.parseExp "True");
      Function (Exp.parseExp "rtimes(a,b)",Rtype.parse "Rational * Rational -> Rational",Exp.parseExp "True");
      Function (Exp.parseExp "ndivide(a,b)",Rtype.parse "Natural * Natural -> Natural",parseExp "not(b==0)");
      Function (Exp.parseExp "rdivide(a,b)",Rtype.parse "Rational * Rational -> Rational",parseExp "not(b==0)");
      Function (Exp.parseExp "nmod(a,b)",Rtype.parse "Natural * Natural -> Natural",parseExp "not(b==0)");
      Function (Exp.parseExp "rmod(a,b)",Rtype.parse "Rational * Rational -> Rational",parseExp "not(b==0)");
      Function (Exp.parseExp "nminus(a,b)",Rtype.parse "Natural * Natural -> Natural",parseExp "not(a<b)");
      Function (Exp.parseExp "rminus(a,b)",Rtype.parse "Rational * Rational -> Rational",parseExp "not(a<b)");
      Function (Exp.parseExp "size(s)",Rtype.parse "String -> Natural",parseExp "True");
      Function (Exp.parseExp "char(s,i)",Rtype.parse "String * Natural -> Char",parseExp "i < size(s)");
      Function (Exp.parseExp "concat(c,s)",Rtype.parse "Char * String -> String",parseExp "True");
      Function (Exp.parseExp "chr(s)",Rtype.parse "Natural -> Char",parseExp "True");
      Function (Exp.parseExp "asc(c)",Rtype.parse "Char -> Natural",parseExp "True");
      Function (Exp.parseExp "if a then b else c",Rtype.parse "Bool * 'a * 'a -> 'a",parseExp "True");
      Function (parseRule "a { b } -> c",Rtype.parse "'a * 'a * Bool -> Unit",Exp.parseExp "True");
      Function (parseRule "a { b } = c",Rtype.parse "'a * 'a * Bool -> Unit",Exp.parseExp "True");
      Function (Exp.parseExp "preceq(a,b)",Rtype.parse "'a * 'a -> Bool",parseExp "True");
      Function (Exp.parseExp "apply(a,b)",Rtype.parse "(('a -> 'b) * 'a) -> 'b",parseExp "True")
     ] ;;

let l2 =
    [parseRule "nminus(x,y)==z = x==nplus(y,z)";
     parseRule "x==nplus(y,z) = nminus(x,y)==z";
     parseRule "x==y { nless(x,y) } = False";
     parseRule "False { nless(x,y) } = x==y";
     parseRule "nless(x,nplus(x,1)) -> True";
     parseRule "True = nless(x,nplus(x,1))";
     parseRule "nminus(x,nplus(a,b)) = nminus(nminus(x,a),b)";
     parseRule "nminus(nminus(x,a),b) = nminus(x,nplus(a,b))";
     parseRule "nminus(x,nminus(a-b)) -> nplus(nminus(x-a),b)";
     parseRule "nplus(nminus(x,a),b) -> nminus(x,nminus(a,b))";
     parseRule "nminus(nplus(x,a),b) = nplus(nminus(x,b),a)";
     parseRule "nplus(nminus(x,b),a)=nminus(nplus(x,a),b)";
     parseRule "preceq(0,y) -> True";
     parseRule "True = preceq(0,y)";
     parseRule "nless(0,nplus(1,x)) -> True";
     parseRule "True = nless(0,nplus(1,x))";
     parseRule "nminus(nplus(a,b),c) { not(nless(c,a)) } = nplus(nminus(a,c),b)";
     parseRule "nplus(nminus(a,c),b) = nminus(nplus(a,b),c)";
     parseRule "nminus(nplus(1,x),nplus(1,y)) -> nminus(x,y)";
     parseRule "nminus(x,y) = nminus(nplus(1,x)-nplus(1,y))";
     parseRule "nless(x,y) | nless(y,x) | x == y -> True";
     parseRule "True = nless(x,y) | nless(y,x) | x == y";
     parseRule "nless(x,y) & nless(y,x) -> False";
     parseRule "False = nless(x,y) & nless(y,x)";
     parseRule "x==y & nless(y,x) -> False";
     parseRule "False = nless(x,y) & y==x";
     parseRule "nless(nplus(a,1),b) { not(nplus(a,1)==b) & nless(a,b) } -> True";
     parseRule "True = nless(nplus(a,1),b) { not(nplus(a,1)==b) & nless(a,b) } -> True";
     parseRule "ntimes(x,nplus(y,z)) -> nplus(ntimes(x,y),ntimes(x,z))";
     parseRule "nplus(ntimes(x,y),ntimes(x,z)) = ntimes(x,nplus(y,z))";
     parseRule "x==nplus(x,1) -> False";
     parseRule "False = x==nplus(x,1)";
     parseRule "nplus(x,y)==nplus(x,z) -> y==z";
     parseRule "y==z = nplus(x,y)==nplus(x,z)";
     parseRule "nless(nplus(x,y),nplus(x,z)) -> nless(y,z)";
     parseRule "nless(y,z) = nless(nplus(x,y),nplus(x,z))";
     parseRule "nless(x,x) -> False";
     parseRule "False = nless(x,x)";
     parseRule "nminus(x,x) -> 0";
     parseRule "0 = nminus(x,x)";
     parseRule "nminus(x,0) -> x";
     parseRule "x = nminus(x,0)";
     parseRule "nminus(x,a)==b -> x==nplus(a,b)";
     parseRule "x==nplus(a,b) = nminus(x,a)==b";
     parseRule "nless(nminus(x,1),y) -> nless(x,nplus(y,1))";
     parseRule "nless(x,nplus(y,1)) = nless(nminus(x,1),y)";
     parseRule "nless(nminus(x,a),b) -> nless(x,nplus(a,b))";
     parseRule "nless(x,nplus(a,b)) = nless(nminus(x,a),b)";
     parseRule "nplus(x,0) = x";
     parseRule "x = nplus(x,0)";
     parseRule "nless(a,nminus(b,x)) -> nless(nplus(a,x),b)";
     parseRule "nless(nplus(a,x),b) = nless(a,nminus(b,x))";
     parseRule "ntimes(x,y)==ntimes(x,z) { not(x==0) } -> y==z";
     parseRule "y==z { not(x==0) } = ntimes(x,y)==ntimes(x,z)";
     parseRule "rminus(x,y)==z = x==rplus(y,z)";
     parseRule "x==rplus(y,z) = rminus(x,y)==z";
     parseRule "x==y { rless(x,y) } = False";
     parseRule "False { rless(x,y) } = x==y";
     parseRule "rless(x,rplus(x,1#1)) -> True";
     parseRule "True = rless(x,rplus(x,1#1))";
     parseRule "rminus(x,rplus(a,b)) = rminus(rminus(x,a),b)";
     parseRule "rminus(rminus(x,a),b) = rminus(x,rplus(a,b))";
     parseRule "rminus(x,rminus(a-b)) -> rplus(rminus(x-a),b)";
     parseRule "rplus(rminus(x,a),b) -> rminus(x,rminus(a,b))";
     parseRule "rminus(rplus(x,a),b) = rplus(rminus(x,b),a)";
     parseRule "rplus(rminus(x,b),a)=rminus(nplus(x,a),b)";
     parseRule "rless(0#1,rplus(1#1,x)) -> True";
     parseRule "True = rless(0,rplus(1#1,x))";
     parseRule "rminus(rplus(a,b),c) { not(rless(c,a)) } = rplus(rminus(a,c),b)";
     parseRule "rplus(rminus(a,c),b) = rminus(rplus(a,b),c)";
     parseRule "rminus(rplus(1#1,x),rplus(1#1,y)) -> rminus(x,y)";
     parseRule "rminus(x,y) = rminus(rplus(1#1,x),rplus(1#1,y))";
     parseRule "rless(x,y) | rless(y,x) | x == y -> True";
     parseRule "True = rless(x,y) | rless(y,x) | x == y";
     parseRule "rless(x,y) & rless(y,x) -> False";
     parseRule "False = rless(x,y) & rless(y,x)";
     parseRule "x==y & rless(y,x) -> False";
     parseRule "False = rless(x,y) & y==x";
     parseRule "rtimes(x,rplus(y,z)) -> rplus(rtimes(x,y),rtimes(x,z))";
     parseRule "nplus(ntimes(x,y),ntimes(x,z)) = ntimes(x,nplus(y,z))";
     parseRule "x==rplus(x,1#1) -> False";
     parseRule "False = x==rplus(x,1#1)";
     parseRule "rplus(x,y)==rplus(x,z) -> y==z";
     parseRule "y==z = rplus(x,y)==rplus(x,z)";
     parseRule "rless(rplus(x,y),rplus(x,z)) -> rless(y,z)";
     parseRule "rless(y,z) = rless(rplus(x,y),rplus(x,z))";
     parseRule "rless(x,x) -> False";
     parseRule "False = rless(x,x)";
     parseRule "rminus(x,x) -> 0#1";
     parseRule "0#1 = rminus(x,x)";
     parseRule "rminus(x,0#1) -> x";
     parseRule "x = rminus(x,0#1)";
     parseRule "rminus(x,a)==b -> x==rplus(a,b)";
     parseRule "x==rplus(a,b) = rminus(x,a)==b";
     parseRule "rless(rminus(x,1#1),y) -> rless(x,rplus(y,1#1))";
     parseRule "rless(x,rplus(y,1#1)) = rless(rminus(x,1),y)";
     parseRule "rless(rminus(x,a),b) -> rless(x,rplus(a,b))";
     parseRule "rless(x,rplus(a,b)) = rless(rminus(x,a),b)";
     parseRule "rplus(x,0#1) = x";
     parseRule "x = rplus(x,0#1)";
     parseRule "rless(a,rminus(b,x)) -> rless(rplus(a,x),b)";
     parseRule "rless(rplus(a,x),b) = rless(a,rminus(b,x))";
     parseRule "rtimes(x,y)==rtimes(x,z) { not(x==0#1) } -> y==z";
     parseRule "y==z { not(x==0#1) } = rtimes(x,y)==rtimes(x,z)"
    ] ;;

let x = List.fold_right (fun p -> (fun env -> addProperty env p)) l2 ;;
let y = List.fold_right (fun i -> (fun t -> addItem t i)) l1 ;;
let z = Disc.newDisc ;;
let p = PrettyPrint.emptyPpenv ;;
let ns = Disc.newSmall ;;

let emptyEn1 = (x
		(y
                 (Array.make 0 []),
                 Disc.newDisc,
                 Disc.newDisc,
                 Disc.newDisc,
                 Array.make 0 [],
                 [],
                 Array.make 0 0,
                 emptyModes,
                 Array.make 0 [],
                 0,
                 [],
                 PrettyPrint.emptyPpenv,(Disc.newSmall,[]),[],[])) ;;

let emptyEn2 = (addAttrib (addAttrib (addAttrib (addAttrib (addAttrib
               (addAttrib (addAttrib (addAttrib (addAttrib (addAttrib
               (addAttrib (addAttrib emptyEn1
                   intern_ac [S(intern_and)])
                   intern_ac [S(intern_and)])
                   intern_ac [S(intern_or)])
                   intern_ac [S(intern_nat_plus)])
                   intern_ac [S(intern_nat_times)])
                   intern_ac [S(intern_rat_plus)])
                   intern_ac [S(intern_rat_times)])
                   intern_c [S(intern_equal)])
                   intern_eq [S(intern_equal)])
                   intern_epo [S(intern_preceq);S(intern_equal)])
                   intern_po [S(intern_nat_less);S(intern_equal)])
                   intern_po [S(intern_rat_less);S(intern_equal)]) ;;

let emptyEnv = emptyEn2 ;;

(*let emptyEnv = addConversion
                   (addOverload
                       (addOverload
                           (addOverload
                               (addOverload
                                   (addOverload
                                       (addOverload emptyEn1 intern_plus [intern_nat_plus,intern_rat_plus])
                                       intern_minus [intern_nat_minus,intern_rat_minus])
                                   intern_star [intern_nat_times,intern_rat_times])
                               intern_slash [intern_nat_divide,intern_rat_divide])
                           intern_percent [intern_nat_mod,intern_rat_mod])
                       intern_less [intern_nat_less,intern_rat_less])
                   (intern_nat_to_rat,intern_nat_to_rat) ;;*)



