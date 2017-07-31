(******************************************************************************
 *
 * REWRITELIB
 *
 * pp.ml
 *
 * Pretty printing
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
(* require "exp.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "disc.sml" ;  *)
(* require "pp-s.sml" ;  *)
(* require "basis.__integer" ;  *)

(* open listimpl ; *)
(* open PARSERimpl ; *)
open Lex ;;
open Exp ;;
(* open INTERNimpl ; *)
(* open SUBSTimpl ; *)
(* open DISCimpl ; *)

(* infix 2 *| ; *)
(* infix 2 <| ; *)
(* infix 2 |> ; *)
(* infix 2 <> ; *)
(* infix 3 >< ; *)

type precedence = LEFT of Lex.token * int | RIGHT of Lex.token * int ;;

type break = ForbidBreak
           | OptionalBreak of int
           | RequiredBreak of int ;;

type directive = ParsePattern of string * int * exp * (token list)
               | PrintPattern of string * int * exp * (token list)
               | InputPattern of string * int * exp * (token list)
               | ParsePermit of string * string
               | ParseBreak of string * (break list) * (bool list)
               | ParsePrecedence of string * precedence
               | ParseMark of string * (int list) * int * int
               ;;

type ppenv = directive list * int list ;;

let print_break b = match b with
  | ForbidBreak -> "forbid"
  | (OptionalBreak i) -> "optional(" ^ (string_of_int i) ^ ")"
  | (RequiredBreak i) -> "required(" ^ (string_of_int i) ^ ")"
  ;;

let rec print_break_list l1 l2 = match (l1,l2) with
  | ([],[]) -> ""
  | ([],(b::r)) ->
    (if b then "true " else "false ") ^ (print_break_list [] r)
  | ((a::r),[]) ->
    (print_break a) ^ " " ^ (print_break_list r [])
  | ((a::r1),(b::r2)) ->
    (print_break a) ^ "-" ^ (if b then "true " else "false ") ^
    (print_break_list r1 r2)
  ;;

let rec prIlist l = match l with
  | [] -> ""
  | [a] -> string_of_int (a:int)
  | (a::b) -> (string_of_int a) ^ "," ^ (prIlist b) ;;

let print_directive p = match p with
  | (ParsePattern (s,i,e,toks)) ->
    "Pattern " ^ s ^ " -> " ^ (string_of_int i) ^ " " ^ (prExp e) ^ ": " ^
    (print_tokens toks)
  | (PrintPattern (s,i,e,toks)) ->
    "PrintPattern " ^ s ^ " -> " ^ (string_of_int i) ^ " " ^ (prExp e) ^ ": " ^
    (print_tokens toks)
  | (InputPattern (s,i,e,toks)) ->
    "InputPattern " ^ s ^ " -> " ^ (string_of_int i) ^ " " ^ (prExp e) ^ ": " ^
    (print_tokens toks)
  | (ParsePermit (m,s)) ->
    "Mode " ^ m ^ " permits " ^ s
  | (ParseBreak (s,bl,bools)) ->
    "Breaks " ^ s ^ " -> " ^ (print_break_list bl bools)
  | (ParseMark (s,il,st,en)) ->
    "Mark(" ^ s ^ ",[" ^ (prIlist il) ^ "]," ^ (string_of_int st) ^ "," ^
    (string_of_int en) ^ ")"
  | _ -> "Other"
  ;;

let get_position sl = ((String.length (List.hd sl)),(List.length sl)-1) ;;

let new_sl = [""]

let add_string (f::r) s = (f ^ s)::r

let add_return sl = ""::sl

let rec make_indent n = match n with
  | 0 -> ""
  | n -> " " ^ (make_indent (n-1))

let add_return_indent sl i = add_string (add_return sl) (make_indent i) ;;

let last_line_length (f::r) = String.length f ;;

(*********************************************************************)

let emptyPpenv = ([],[])

let addDirective (d,r) dd = (dd::d,r)

let get_directives (d,_) = d

let usable_in_mode ppenv m s =
    let rec uim x = match x with
          | [] -> false
          | (ParsePermit (mm,ss)::r) ->
            if mm=m && ss=s then
                true
            else
                uim r
          | (_::r) -> uim r
    in
          uim (get_directives ppenv)
    ;;

let get_parse_marks ppenv name =
    let rec gpm x = match x with
          | [] -> []
          | (ParseMark (rule,il,s,e)::r) ->
            if rule=name then
                (il,s,e)::(gpm r)
            else
                gpm r
          | (_::r) -> gpm r
    in
          gpm (get_directives ppenv)
    ;;

let get_parse_breaks ppenv name =
    let rec gpb x = match x with
          | [] -> []
          | (ParseBreak (rule,il,bl)::r) ->
            if rule=name then
                (il,bl)::(gpb r)
            else
                gpb r
          | (_::r) -> gpb r
    in
          gpb (get_directives ppenv)
    ;;

(*********************************************************************)

let rec has_match p e = match (p,e) with
  | ((VAR x),_) -> true
  | ((APPL (f1,l1)),(APPL (f2,l2))) ->
    if f1=f2 && List.length l1=List.length l2 then
        has_match_list l1 l2
    else false
  | (_,_) -> false
and has_match_list l1 l2 = match (l1,l2) with
  | ([],[]) -> true
  | ((a::b),(c::d)) -> (has_match a c) && (has_match_list b d)
  | (_,_) -> false
  ;;

exception FailedMatch ;;

let rec u_match s pos p e = match (p,e) with
  | ((VAR x),y) -> (x,pos,y)::s
  | ((APPL (f1,l1)),(APPL (f2,l2))) ->
    if f1=f2 && List.length l1 = List.length l2 then
        u_match_list s pos 0 l1 l2
    else
        raise FailedMatch
and u_match_list s pos n l1 l2 = match (l1,l2) with
  | ([],[]) -> s
  | ((a::b),(c::d)) ->
    u_match_list (u_match s (n::pos) a c) pos (n+1) b d
  ;;

let mmatch a b = u_match [] [] a b ;;

exception SubstDomain ;;

let rec getExp l v = match l with
  | [] -> raise SubstDomain
  | ((x,pos,e)::r) ->
    if x=v then e else getExp r v
  ;;

let rec getPos l v = match l with
  | [] -> raise SubstDomain
  | ((x,pos,e)::r) ->
    if x=v then pos else getPos r v
  ;;

(*********************************************************************)

exception FailedPrint ;;

let rec add_string_limit sl s wid force =
    let sl2 = add_string sl s in
        if not(force) && last_line_length sl2 > wid then raise FailedPrint else sl2
    ;;

let rec split_pattern l = match l with
  | [] -> ([],[])
  | (TVAR ("","",0)::r) -> ([],r)
  | (f::r) -> let (l1,l2) = split_pattern r
    in
        (f::l1,l2)
  ;;

let rec last l = match l with
  | [x] -> x
  | (a::b) -> last b
  ;;

let rec strip_last l = match l with
  | [x] -> []
  | (a::b) -> a::(strip_last b)
  ;;

let exp_replace_with_suffix e fv n =
    let rec ms l = match l with
          | [] -> Subst.empty
          | (a::b) -> Subst.addPair (ms b) a
                     (VAR (Intern.intern ((Intern.decode a) ^ "_" ^ (string_of_int n))))
    in
        Subst.subst (ms fv) e
    ;;

let rec token_replace_with_suffix l fv n = match l with
  | [] -> []
  | (TVAR (v,m,i)::r) ->
    TVAR((if List.mem (Intern.intern v) fv then v ^ "_" ^ (string_of_int (n:int)) else v),m,i)::
    token_replace_with_suffix r fv n
  | (f::r) -> f::(token_replace_with_suffix r fv n)
  ;;

let rec repeat_front l1 l2 n m count = match (l2,m,count) with
  | (l2,m,0) -> l2
  | (l2,0,count) -> if count < 0 then l2 else repeat_front l1 l1 n n (count-1)
  | ((a::b),m,count) ->
    a::(repeat_front l1 b m (n-1) count) ;;

let rec repeat_center l m n count = match (l,m) with
  | (l,0) -> repeat_front l l n n count
  | ((a::b),m) -> a::(repeat_center b (m-1) (n-1) count)
  ;;

let rec extend l n = match (l,n) with
  | ([a],0) -> []
  | ([a],n) -> if n<0 then [] else a::(extend [a] (n-1))
  | ((a::b),n) -> a::(extend b (n-1))
  ;;

let rec remove_empties l = match l with
  | [] -> []
  | ((TVAR ("","",i))::r) -> remove_empties r
  | (f::r) -> f::(remove_empties r)
  ;;

let stretch (APPL (f,l)) (ParsePattern (s,p,(APPL (f2,l2)),tl),breaks) =
    if List.length l=List.length l2 then
        (ParsePattern (s,p,(APPL (f2,l2)),remove_empties tl),breaks)
    else match(split_pattern tl) with
       | (_,[]) -> (ParsePattern (s,p,(APPL (f2,l2)),tl),breaks)
       | _ ->
                 (*val _ = print ("Stretching " ^ (prExp (APPL (f,l))) ^ " " ^ (prExp (APPL (f2,l2))) ^ " " ^ (print_tokens tl) ^ "\n")*)
        let (pre,mid) = split_pattern tl in
        let (middle,post) = split_pattern mid in
        let ll = last l2 in
        let fv = Context.getFreeVars ll in
        let repeat_count = (List.length l)-(List.length l2)+1 in
        let rec replicate_exp n = match n with
              | 0 -> []
              | n ->
                (exp_replace_with_suffix ll fv (repeat_count-n))::(replicate_exp (n-1)) in
        let new_l2 = (strip_last l2)@(replicate_exp repeat_count) in
        let rec replicate_tokens n = match n with
          | 0 -> []
          | n ->
            (token_replace_with_suffix middle fv (repeat_count-n))@(replicate_tokens (n-1)) in
        let new_tl = pre@(replicate_tokens repeat_count)@post in
        let b1 = List.length pre in
        let b2 = b1+(List.length middle) in
        let new_breaks = List.map
            (fun (ol,bl) ->
                 (repeat_center (extend ol b2) b1 b2 repeat_count,
                  repeat_center (extend bl b2) b1 b2 repeat_count))
            breaks
        (*val _ = print ("result " ^ (prExp (APPL (f2,new_l2))) ^ " " ^ (print_tokens new_tl) ^ "\n")*)
    in
        (ParsePattern (s,p,(APPL (f2,new_l2)),new_tl),new_breaks) ;;

let defaults = ["default";"identifier";"natural"] ;;

let int_get_patterns ppenv exp mode prec =
    let rec same_functor e1 e2 = match (e1,e2) with
          | ((APPL (f1,l1)),(APPL (f2,l2))) -> f1=f2
          | (_,_) -> false in
    let rec non_appl e = match e with
          | (APPL (f,l)) -> false
          | _ -> true in
    let patterns = List.filter
                     (fun (x) -> match x with
                                  | ParsePattern (s,p,e,tl) ->
                                         (prec < p) &&
                                         same_functor e exp &&
                                         usable_in_mode ppenv mode s
                                  | _ -> false)
                     (get_directives ppenv) in
    let patterns3 = List.map
                    (fun (ParsePattern (s,p,e,b)) ->
                         (ParsePattern (s,p,e,b),get_parse_breaks ppenv s))
                    (List.filter
                     (fun (x) -> match x with
                                  | ParsePattern (s,p,e,tl) ->
                                         prec < p &&
                                         non_appl e &&
                                         not(List.mem mode defaults) &&
                                         usable_in_mode ppenv mode s &&
                                         has_match e exp
                                  | _ -> false) (get_directives ppenv)) in
    let rec first_stretch_match l = match l with
          | [] -> []
          | (f::r) ->
            let ParsePattern (s,_,_,_) = f in
            let breaks = get_parse_breaks ppenv s in
            let (ParsePattern (s,p,e,b),breaks) = stretch exp (f,breaks) in
                if has_match e exp then
                    [(ParsePattern (s,p,e,b),breaks)]
                else
                    first_stretch_match r in
    let p = first_stretch_match patterns in
        if List.length p > 0 then
            [List.hd p]
        else if List.length patterns3 > 0 then
            [List.hd patterns3]
                else
            []
    ;;

let p_cache = ref Disc.newExpItem (*: ((string * int * (directive * (((Break list) * (bool list)) list)) list) ExpDisc) ref*)

let get_patterns ppenv exp mode prec =
    let res = List.filter (fun (s,i,dl) -> s=mode && i=prec) (Disc.findExpItem (!p_cache) exp)
    in
        if res=[] then
            let dl = int_get_patterns ppenv exp mode prec
            in
                 p_cache := Disc.addExpItem (!p_cache) exp (mode,prec,dl) ;
                 dl
        else
            let (s,i,dl) = (List.hd res) in dl
    ;;

let rec print_breaks b = match b with
  | [] -> ""
  | (ForbidBreak::r) -> "ForbidBreak " ^ (print_breaks r)
  | (OptionalBreak i::r) -> "OptionalBreak " ^ (string_of_int i) ^ " " ^ (print_breaks r)
  | (RequiredBreak i::r) -> "RequiredBreak " ^ (string_of_int i) ^ " " ^ (print_breaks r)
  ;;

let rec recPpExp ppenv exp wid one_line mode sl prefix markers indent failup prec force =
    (*let val patterns = get_directives ppenv
                     |> (fn (x) => case x of ParsePattern (s,p,e,tl) => prec < p && usable_in_mode ppenv mode s && has_match e exp | _ => false)*)
    let patterns = get_patterns ppenv exp mode prec in
        (*val _ = print ("exp = " ^ (prExp exp) ^ " " ^ (string_of_int indent) ^ "\n")
        val _ = print ("pattern length = " ^ (string_of_int (length patterns)) ^ "\n")*)
    let (start_x,start_y) = get_position sl in
    let rec pe l = match l with
          | [] -> (try defaultPpProcess ppenv exp wid one_line mode sl prefix markers indent force
                      with FailedPrint -> if failup then raise FailedPrint else (add_return (add_string sl "*No parse*"),markers))
          | (f::r) -> (try recPpProcess ppenv exp wid one_line sl prefix markers indent f force
                      with FailedPrint -> if failup then raise FailedPrint else (add_return (add_string sl "*No parse*"),markers)) in
    let (new_sl,new_markers) = pe patterns in
    let (end_x,end_y) = get_position new_sl in
        (new_sl,(prefix,start_x,start_y,end_x,end_y)::new_markers)
and recPpProcess ppenv exp wid one_line sl prefix markers indent (ParsePattern (s,prec,e,p),parse_breaks) force =
        (*val _ = print ("***** rppb starting " ^ (prExp exp) ^ " " ^ (prExp e) ^ " " ^ (print_tokens p) ^ "\n")*)
    let parse_marks = get_parse_marks ppenv s in
    let rec rppb l = match l with
          | [] -> raise FailedPrint
          | (a::b) -> try recPpProcessBreak ppenv exp wid one_line sl prefix markers indent (ParsePattern (s,prec,e,p)) a parse_marks (force && b=[])
                            with FailedPrint -> rppb b
    in
        rppb (if one_line && List.length parse_breaks > 0 then [([ForbidBreak],[false])] else parse_breaks)
and recPpProcessBreak ppenv exp wid one_line sl prefix markers indent (ParsePattern (s,prec,e,p)) (ol,bl) parse_marks force =
    (*let val _ = print ("Testing " ^ (prExp e) ^ " " ^ (print_tokens p) ^ " " ^ (prExp exp) ^ " " ^ (print_breaks ol) ^ "\n")*)
    let rec build_holders l = match l with
          | [] -> []
          | ((il,s,e)::r) ->
            (il@prefix,0,0,0,0)::(build_holders r) in
    let holders = build_holders parse_marks in
    let rec update_holder sl pos (p,sx,sy,ex,ey) (il,s,e) =
            let (nsx,nsy) = if pos=s then get_position sl else (sx,sy) in
            let (nex,ney) = if pos=e then get_position sl else (ex,ey) in
                (p,nsx,nsy,nex,ney) in
    let rec update_holders sl pos l1 l2 = match (l1,l2) with
          | ([],[]) -> []
          | ((a::b),(c::d)) ->
            (update_holder sl pos a c)::(update_holders sl pos b d) in
    let next f r = if r=[] then [f] else r in
    let theta = mmatch e exp in
    let rec process sl holders markers n l1 l2 l3 = match (l1,l2,l3) with
          | ([],_,_) ->
            let new_holders = update_holders sl n holders parse_marks in
            let rec holders_2_markers l = match l with
                  | [] -> []
                  | ((p,sx,sy,ex,ey)::r) ->
                    ((p@prefix,sx,sy,ex,ey)::(holders_2_markers r)) in
            let markers = (holders_2_markers new_holders)@markers in
                (sl,markers)
          | ((tf::tr),(olf::olr),(blf::blr)) ->
            let new_holders = update_holders sl n holders parse_marks in
            let rec add_tok sl marksers t force = match t with
                       | (ID s) -> (add_string_limit sl s wid force,markers)
                       | (SYMBOL s) -> (add_string_limit sl s wid force,markers)
                       | (NUMBER n) -> (add_string_limit sl (string_of_int n) wid force,markers)
                       | (SPECIAL s) -> (add_string_limit sl s wid force,markers)
                       | (TVAR (v,m,prec)) -> recPpExp ppenv (getExp theta (Intern.intern v)) wid (blf || one_line) m sl ((getPos theta (Intern.intern v))@prefix) markers indent true prec force in
            let rec add_nl_tok sl marksers indent t force = match t with
                       | (ID s) -> (add_string_limit sl s wid force,markers)
                       | (SYMBOL s) -> (add_string_limit sl s wid force,markers)
                       | (NUMBER n) -> (add_string_limit sl (string_of_int n) wid force,markers)
                       | (SPECIAL " ") -> (sl,markers)
                       | (SPECIAL s) -> (add_string_limit sl s wid force,markers)
                       | (TVAR (v,m,prec)) -> recPpExp ppenv (getExp theta (Intern.intern v)) wid (blf || one_line) m sl ((getPos theta (Intern.intern v))@prefix) markers indent true prec force in
            let ((new_sl,new_markers),new_ind) =
                    if one_line then
                        (add_tok sl markers tf force,indent)
                    else match olf with
                           | ForbidBreak -> (add_tok sl markers tf force,indent)
                           | OptionalBreak(nn) -> (try (add_tok sl markers tf false,indent)
                                                   with FailedPrint -> ((add_nl_tok (add_return_indent sl (indent+nn)) markers (indent+nn) tf force),indent+nn))
                           | RequiredBreak(nn) -> (add_nl_tok (add_return_indent sl (indent+nn)) markers (indent+nn) tf force,indent+nn)
            in
                process new_sl new_holders new_markers (n+1) tr (next olf olr) (next blf blr)
    in
        process sl holders markers 0 p ol bl
and defaultPpProcess ppenv e wid one_line t sl prefix markers indent force = match (e,t) with
  | ((STRING s),"identifier") ->
    (add_string_limit sl s wid force, markers)
  | ((NUM n),"natural") ->
    (add_string_limit sl (string_of_int n) wid force, markers)
  | ((VAR x),"default") ->
    (add_string_limit sl (Intern.decode x) wid force, markers)
  | ((MARKED_VAR x),"default") ->
    (add_string_limit sl ((Intern.decode x) ^ "'") wid force, markers)
  | ((NUM n),"default") ->
    (add_string_limit sl (string_of_int n) wid force,
     markers)
  | ((STRING s),"default") ->
    (add_string_limit sl ("\"" ^ s ^ "\"") wid force,
     markers)
  | ((CHAR s),"default") ->
    (add_string_limit sl ("'" ^ (String.make 1 s) ^ "'") wid force,
     markers)
  | ((APPL (f,l)),"default") ->
    (try let sl1 = add_string_limit sl ((Intern.decode f) ^ (if List.length l > 0 then "(" else "") ^ (if (List.length l)=0 && ((Intern.decode f) <"A" || (Intern.decode f) > "ZZZZ") then "()" else "")) wid force in
        (*val _ = print ("*** Default on " ^ (prExp (APPL (f,l))) ^ "\n") ;*)
    let rec add_p sl markers n l = match l with
          | [] -> (sl,markers)
          | [a] ->
            let (sl1,markers1) = recPpExp ppenv a wid true "default" sl (n::prefix) markers indent true 0 false in
            let sl2 = add_string_limit sl1 ")" wid false in
                (sl2,markers1)
          | (a::b) ->
            let (sl1,markers1) =
                     recPpExp ppenv a wid true "default" sl (n::prefix) markers indent true 0 false in
            let sl2 = add_string_limit sl1 ", " wid false in
                add_p sl2 markers1 (n+1) b in
    let  (sl2,markers2) = add_p sl1 markers 0 l in
        (sl2,markers2)
    with FailedPrint -> if one_line then
        raise FailedPrint
    else let sl1 = add_string_limit sl ((Intern.decode f) ^ (if List.length l > 0 then "(" else "") ^ (if List.length l=0 && (((Intern.decode f) < "A" ||  (Intern.decode f) > "ZZZ")) then "()" else "")) wid force in
         let rec add_p sl markers n l = match l with
               | [] -> (sl,markers)
               | [a] ->
                 let sl1 = add_return_indent sl (indent+2) in
                 let (sl2,markers1) = recPpExp ppenv a wid false "default" sl1 (n::prefix) markers (indent+2) false 0 force in
                 let sl3 = add_string_limit sl2 ")" wid force in
                     (sl3,markers1)
               | (a::b) ->
                 let sl1 = add_return_indent sl (indent+2) in
                 let (sl2,markers1) = recPpExp ppenv a wid false "default" sl1 (n::prefix) markers (indent+2) false 0 force in
                 let sl3 = add_string_limit sl2 "," wid force in
                     add_p sl3 markers1 (n+1) b in
             let (sl2,markers2) = add_p sl1 markers 0 l
         in
             (sl2,markers2)
         )
  | ((QUANT (15,v,e,p)),mode) ->
    (try let sl1 = add_string_limit sl "EXISTS(" wid false in
     let sl2 = varPpProcess v wid false sl1 in
     let sl3 = add_string_limit sl2 ") " wid false in
        recPpExp ppenv e wid true "default" sl3 (0::prefix) markers (indent+2) true 0 false
    with FailedPrint -> if one_line then
        raise FailedPrint
    else let sl1 = add_string_limit sl "EXISTS(" wid force in
        let sl2 = varPpProcess v wid force sl1 in
        let sl3 = add_return_indent (add_string_limit sl2 ")" wid force) (indent+2)
    in
        recPpExp ppenv e wid false "default" sl3 (0::prefix) markers (indent+2) false 0 force
    )
  | ((QUANT (14,v,e,p)),mode) ->
    (try let sl1 = add_string_limit sl "ALL(" wid false in
     let sl2 = varPpProcess v wid false sl1 in
     let sl3 = add_string_limit sl2 ": " wid false in
     let (sl4,markers1) = recPpExp ppenv p  wid true "default" sl3 (1::prefix) markers (indent+2) true 0 false in
     let sl5 = add_string_limit sl4 ") " wid false in

     let (sl6,markers2) = recPpExp ppenv e wid true "default" sl5 (0::prefix) markers1 (indent+2) true 0 false in
        (sl6,markers1@markers2)
    with FailedPrint -> if one_line then
        raise FailedPrint
    else let sl1 = add_string_limit sl "ALL(" wid force in
         let sl2 = varPpProcess v wid force sl1 in
         let sl3 = add_return_indent (add_string_limit sl2 ":" wid force) (indent+2) in
         let (sl4,markers1) = recPpExp ppenv p  wid false "default" sl3 (1::prefix) markers (indent+2) false 0 force in
         let sl5 = add_return_indent (add_string_limit sl4 ")" wid force) (indent+2) in
         let (sl6,markers2) = recPpExp ppenv e wid false "default" sl5 (0::prefix) markers1 (indent+2) false 0 force in
        (sl6,markers1@markers2)
    )
  | (_,_) ->
    raise FailedPrint
and varPpProcess l wid force sl = match l with
  | [] -> sl
  | [(v,t)] ->
    if Type.notetype t then add_string_limit sl (Intern.decode v) wid force
                   else add_string_limit sl ((Intern.decode v) ^ ":" ^ (Type.unparse t)) wid force
  | ((v,t)::r) ->
    varPpProcess r wid force
       (if Type.notetype t then add_string_limit sl (Intern.decode v) wid force
        else add_string_limit sl ((Intern.decode v) ^ ":" ^ (Type.unparse t)) wid force)
  ;;

let rec filter_print l = match l with
  | [] -> []
  | ((InputPattern (t,i,e,l))::r) -> filter_print r
  | ((PrintPattern (t,i,e,l))::r) ->
    (ParsePattern (t,i,e,l))::(filter_print r)
  | (a::b) -> a::(filter_print b)
  ;;

let rec filter_input l = match l with
  | [] -> []
  | ((PrintPattern (t,i,e,l))::r) -> filter_input r
  | ((InputPattern (t,i,e,l))::r) ->
    (ParsePattern (t,i,e,l))::(filter_input r)
  | (a::b) -> a::(filter_input b)
  ;;

let ppExp ppenv exp wid =
    let xxx = p_cache := Disc.newExpItem in
    let (f,r) = ppenv in
    let ppenv = (filter_print f,r) in
    let (sl,markers) = recPpExp ppenv exp wid false "default" new_sl [] [] 0 false 0 true in
        (sl,List.map (fun (a,b,c,d,e) -> (List.rev a,b,c,d,e)) markers)
    ;;

let ppOneLine ppenv exp =
    let (f,r) = ppenv in
    let ppenv = (filter_print f,r) in
    let (sl,markers) = recPpExp ppenv exp 10000 true "default" new_sl [] [] 0 false 0 true in
        (List.hd sl)
    ;;

let split t =
    let rec spl n f l = match l with
          | [] -> raise (Failure t)
          | ((SPECIAL "(")::r) ->
            spl (n+1) ((SPECIAL "(")::f) r
          | ((SPECIAL "{")::r) ->
            spl (n+1) ((SPECIAL "{")::f) r
          | ((SPECIAL "[")::r) ->
            spl (n+1) ((SPECIAL "[")::f) r
          | ((SPECIAL ")")::r) ->
            spl (n-1) ((SPECIAL ")")::f) r
          | ((SPECIAL "}")::r) ->
            spl (n-1) ((SPECIAL "}")::f) r
          | ((SPECIAL "]")::r) ->
            spl (n-1) ((SPECIAL "]")::f) r
          | ((SPECIAL ":")::r) ->
            if n=0 then
                (List.rev f,r)
            else
                spl n ((SPECIAL ":")::f) r
          | (fr::r) -> spl n (fr::f) r
    in
        spl 0 [] t
    ;;

let process_tokens l = match l with
  | (escape::r) ->
    let rec pt l = match l with
          | [] -> []
          | (f::r) ->
            if f=escape then
                pe r
            else
                f::(pt r)
        and pe l = match l with
          | ((ID name)::(ID mode)::(NUMBER prec)::_::r) ->
            (TVAR (name,mode,prec))::(pt r)
          | ((ID name)::(ID mode)::_::r) ->
            (TVAR (name,mode,0))::(pt r)
          | (f::r) ->
            if f = escape then
                (TVAR ("","",0))::(pt r)
            else
                pt (f::r)
          | t -> raise (Failure t)
    in
        pt r
  | x -> raise (Failure x) ;;

let process_break name l = match l with
  | (escape::r) ->
    let rec pb ol bl l = match l with
          | [] -> ([],[])
          | (f::r) ->
            if f=escape then
                 pe ol bl r
            else let (f,b) = pb ol bl r
                 in
                     (ol::f,bl::b)
        and pe ol bl l = match l with
          | ((ID "fbm")::r) ->
            pe ForbidBreak bl r
          | ((ID "obm")::(NUMBER i)::r) ->
            pe (OptionalBreak i) bl r
          | ((ID "rbm")::(NUMBER i)::r) ->
            pe (RequiredBreak i) bl r
          | ((ID "fb")::_::s::r) ->
            let (olr,blr) = pb ol bl r in
                (ForbidBreak::olr,bl::blr)
          | ((ID "ob")::(NUMBER i)::_::s::r) ->
            let (olr,blr) = pb ol bl r in
                (OptionalBreak i::olr,bl::blr)
          | ((ID "rb")::(NUMBER i)::_::s::r) ->
            let (olr,blr) = pb ol bl r in
                (RequiredBreak i::olr,bl::blr)
          | ((ID "ml")::r) ->
            pe ol false r
          | ((ID "ol")::r) ->
            pe ol true r
          | (_::r) -> pb ol bl r
          | t -> raise (Failure t) in
        let (ol,bl) = pb ForbidBreak true r
    in
        ParseBreak (name,ol,bl)
  | x -> raise (Failure x) ;;

let parse_pattern tokens =
    let (name,prec,r) = match tokens with
                               | ((ID n)::(NUMBER i)::r) -> (n,i,r)
                               | ((ID n)::r)             -> (n,10000,r)
                               | _      -> raise (Failure tokens) in
    let (e_toks,p_toks) = split r in
    let _ = tokenParseExp e_toks in
        ParsePattern (name,prec,tokenParseExp e_toks,process_tokens p_toks) ;;

let rec parse_int_list l = match l with
  | [NUMBER n] -> [n]
  | ((NUMBER n)::(SPECIAL ",")::r) -> n::(parse_int_list r)
  | t -> raise (Failure t)
  ;;

let rec build_default_breaks l n = match l with
  | [] -> []
  | (SPECIAL " "::SPECIAL " "::SPECIAL " "::TVAR("","",i)::r) ->
    (ForbidBreak::ForbidBreak::ForbidBreak::build_default_breaks r n)
  | (SPECIAL " "::SPECIAL " "::TVAR("","",i)::r) ->
    (ForbidBreak::ForbidBreak::build_default_breaks r n)
  | (SPECIAL " "::TVAR("","",i)::r) ->
    (ForbidBreak::build_default_breaks r n)
  | (TVAR("","",i)::r) ->
    (build_default_breaks r n)
  | (SPECIAL " "::SPECIAL " "::SPECIAL " "::TVAR(v,m,i)::r) ->
    (RequiredBreak n::ForbidBreak::ForbidBreak::ForbidBreak::build_default_breaks r n)
  | (SPECIAL " "::SPECIAL " "::TVAR(v,m,i)::r) ->
    (RequiredBreak n::ForbidBreak::ForbidBreak::build_default_breaks r n)
  | (SPECIAL " "::TVAR(v,m,i)::r) ->
    (RequiredBreak n::ForbidBreak::build_default_breaks r n)
  | (TVAR(v,m,i)::r) ->
    (RequiredBreak n::build_default_breaks r n)
  | (f::r) ->
    ForbidBreak::build_default_breaks r n
  ;;

let rec build_def_breaks l n = match l with
  | [] -> []
  | (SPECIAL " "::SPECIAL " "::SPECIAL " "::TVAR("","",i)::r) ->
    (ForbidBreak::ForbidBreak::ForbidBreak::build_def_breaks r n)
  | (SPECIAL " "::SPECIAL " "::TVAR("","",i)::r) ->
    (ForbidBreak::ForbidBreak::build_def_breaks r n)
  | (SPECIAL " "::TVAR("","",i)::r) ->
    (ForbidBreak::build_def_breaks r n)
  | (TVAR("","",i)::r) ->
    (build_def_breaks r n)
  | (SPECIAL " "::SPECIAL " "::SPECIAL " "::TVAR(v,m,i)::r) ->
    (ForbidBreak::ForbidBreak::ForbidBreak::ForbidBreak::build_default_breaks r n)
  | (SPECIAL " "::SPECIAL " "::TVAR(v,m,i)::r) ->
    (ForbidBreak::ForbidBreak::ForbidBreak::build_default_breaks r n)
  | (SPECIAL " "::TVAR(v,m,i)::r) ->
    (ForbidBreak::ForbidBreak::build_default_breaks r n)
  | (TVAR(v,m,i)::r) ->
    (ForbidBreak::build_default_breaks r n)
  | (f::r) ->
    ForbidBreak::build_def_breaks r n

let parseDirective s =
    let t = tokenize s
    in
        match t with
           | ((ID "pattern")::r)           -> [parse_pattern r]
           | ((ID "input")::(ID m)::r)           ->
                 let (ParsePattern (s,i,e,tl)) = parse_pattern r
                 in
                     [InputPattern (s,i,e,tl);
                      ParsePermit (m,s);
                      ParseBreak (s,build_def_breaks tl 2,[false]);
                      ParseBreak (s,[ForbidBreak],[true])]
           | ((ID "output")::(ID m)::r)           ->
                 let (ParsePattern (s,i,e,tl)) = parse_pattern r
                 in
                     [PrintPattern (s,i,e,tl);
                      ParsePermit (m,s);
                      ParseBreak (s,build_def_breaks tl 2,[false]);
                      ParseBreak (s,[ForbidBreak],[true])]
           | ((ID "default")::(ID m)::r)           ->
                 let (ParsePattern (s,i,e,tl)) = parse_pattern r
                 in
                     [ParsePattern (s,i,e,tl);
                      ParsePermit (m,s);
                      ParseBreak (s,build_def_breaks tl 2,[false]);
                      ParseBreak (s,[ForbidBreak],[true])]
           | ((ID "mode")::(ID m)::st::en::r)           ->
                 let (ParsePattern (s,i,e,tl)) = parse_pattern r
                 in
                     [ParsePattern (s,i,e,tl);
                      ParsePermit (m,s);
                      ParseBreak (s,build_def_breaks tl 2,[false]);
                      ParseBreak (s,[ForbidBreak],[true]);
                      PrintPattern ((s ^ "_d"),i,e,st::(tl@[en]));
                      ParsePermit ("default",(s ^ "_d"));
                      ParseBreak ((s ^ "_d"),build_def_breaks (st::(tl@[en])) 2,[false]);
                      ParseBreak ((s ^ "_d"),[ForbidBreak],[true])]
           | ((ID "infix")::(ID m)::t1::t2::r)           ->
                 let (ParsePattern (s,i,e,tl)) = parse_pattern r in
                 let tl2 = t1::(tl@[t2]) in
                 let s2 = s ^ "_p" in
                     [PrintPattern (s2,10000,e,tl2);
                      ParseBreak (s2,build_def_breaks tl2 0,[false]);
                      ParseBreak (s2,[ForbidBreak],[true]);
                      ParsePattern (s,i,e,tl);
                      ParseBreak (s,build_def_breaks tl 0,[false]);
                      ParseBreak (s,[ForbidBreak],[true]);
                      ParsePermit (m,s);
                      ParsePermit (m,s2)]
           | ((ID "infixMode")::(ID m)::st::en::t1::t2::r) ->
                 let (ParsePattern (s,i,e,tl)) = parse_pattern r in
                 let tl2 = t1::(tl@[t2]) in
                 let s2 = s ^ "_p" in
                     [PrintPattern (s2,10000,e,tl2);
                      ParseBreak (s2,build_def_breaks tl2 0,[false]);
                      ParseBreak (s2,[ForbidBreak],[true]);
                      ParsePattern (s,i,e,tl);
                      ParseBreak (s,build_def_breaks tl 0,[false]);
                      ParseBreak (s,[ForbidBreak],[true]);
                      ParsePermit (m,s);
                      ParsePermit (m,s2);
                      PrintPattern ((s2 ^ "_d"),10000,e,st::(tl2@[en]));
                      ParseBreak ((s2 ^ "_d"),build_def_breaks (st::(tl2@[en])) 0,[false]);
                      ParseBreak ((s2 ^ "_d"),[ForbidBreak],[true]);
                      PrintPattern ((s ^ "_d"),i,e,st::(tl@[en]));
                      ParseBreak ((s ^ "_d"),build_def_breaks (st::(tl@[en])) 0,[false]);
                      ParseBreak ((s ^ "_d"),[ForbidBreak],[true]);
                      ParsePermit ("default",(s ^ "_d"));
                      ParsePermit ("default",(s2 ^ "_d"))]
           | [(ID "permit");(ID m);(ID r)] -> [ParsePermit (m,r)]
           | ((ID "break")::(ID name)::r)  -> [process_break name r]
           | ((ID "mark")::(ID name)::(NUMBER s)::(NUMBER e)::r)
                                           -> [ParseMark (name,parse_int_list r,s,e)]
           | _                             -> raise (Failure t)
    ;;

let addParseDirectives pp s =
    List.fold_left (fun b -> (fun a -> addDirective b a)) pp (parseDirective s)
    ;;

(***********************************************************************)


type trie = TrieToken of Lex.token * (trie list)
          | TrieResult of (int * exp * (Lex.token list)) ;;

type trieSet = (string * (trie list)) list ;;

let rec get_trie mode l = match l with
  | [] -> []
  | ((m,tl)::r) ->
    if mode=m then tl else get_trie mode r ;;

let rec indent n = match n with
  | 0 -> ""
  | n -> " " ^ (indent (n-1)) ;;

let rec print_trie i t = match t with
  | (TrieToken (t,tl)) ->
    (indent i) ^ "Case " ^ (print_tokens [t]) ^ "\n" ^
    (List.fold_right ( ^ ) (List.map (print_trie (i+2)) tl) "")
  | (TrieResult (p,e,tl)) ->
    (indent i) ^ "Result " ^ (string_of_int p) ^ " " ^ (prExp e) ^ " " ^
    (print_tokens tl) ^ "\n" ;;

let rec print_trie_set l = match l with
  | [] -> ""
  | ((s,tl)::r) ->
    "Mode: " ^ s ^ "\n" ^ (List.fold_right ( ^ ) (List.map (print_trie 2) tl) "") ^
    (print_trie_set r) ;;

let build_trie patterns mode =
    let sequences = List.fold_left List.append [] (List.map (fun x -> match x with | (ParsePattern (s,i,e,tl)) ->
                        if usable_in_mode (patterns,[]) mode s then
                            [(i,e,tl,[])]
                        else [] | _ -> []) patterns) in
    let rec similar_token t1 t2 = match (t1,t2) with
          | ((TVAR (s1,s2,i1)),(TVAR (t1,t2,i2))) ->
            s2=t2 && i1=i2
          | (x,y) -> x=y in
    let rec has_similar tok l = match l with
          | [] -> false
          | (a::b) ->
            (similar_token tok a) || (has_similar tok b) in
    let rec remove_similar_dups l = match l with
          | [] -> []
          | (a::b) ->
            if has_similar a b then
                remove_similar_dups b
            else
                a::(remove_similar_dups b) in
    let rec rename_token n1 n2 t = match t with
          | (TVAR (s1,s2,prec)) ->
            if s1 = n1 then
                (TVAR (n2,s2,prec))
            else
                (TVAR (s1,s2,prec))
          | x -> x in
    let rename_var n1 n2 (i,e,tl,rest) =
        let u = Subst.addPair Subst.empty (Intern.intern n1) (VAR (Intern.intern n2))
        in
            (i, Subst.subst u e, List.map (rename_token n1 n2) tl, rest) in
    let rec rv count t = match t with
          | (i,e,(TVAR (s1,s2,prec)::r),rest) ->
            rename_var s1 ("v" ^ (string_of_int count)) (i,e,(TVAR (s1,s2,prec)::r),rest)
          | x -> x in
    let rec make_name tv count = match tv with
          | (TVAR (n,m,prec)) -> (TVAR (("v" ^ (string_of_int count)),m,prec))
          | t -> t in
    let rec ss l = match l with
          | l -> []
          | (SPECIAL " "::r) -> ss r
          | (a::b) -> a::(ss b) in
    let rec strip_spaces (i,e,toks,rest) =
        (i,e,ss toks,rest) in
    let rec carry_ac_stuff s = match s with
          | (i,e,(TVAR ("","",0))::f::r,[]) ->
            (i,e,f::r,[(TVAR ("","",0));f])
          | (i,e,(TVAR ("","",0))::r,x) ->
            (i,e,[],x@((TVAR ("","",0))::r))
          | (i,e,x,[]) -> (i,e,x,[])
          | (i,e,(f::r),x) -> (i,e,(f::r),x@[f]) in
    let rec btt token count seqs =
            let seqs2 = List.filter (fun (i,e,toks,rest) -> (not(toks=[]) &&
                                     similar_token token (List.hd toks))) seqs in
            let seqs3 = List.map (fun (i,e,toks,rest) -> (i,e,List.tl toks,rest)) (List.map (rv count) seqs2)
            in
                TrieToken (make_name token count,bt (count+1) seqs3)
        and bt count seqs =
            let seqs = List.map carry_ac_stuff (List.map strip_spaces seqs) in
            let exps = List.map (fun (i,e,toks,rest) -> TrieResult (i,e,rest))
                                (List.filter (fun (i,e,toks,rest) -> toks=[]) seqs) in
            let nseqs = (List.map
                             (fun (i,e,toks,rest) -> (List.hd toks))
                             (List.filter (fun (i,e,toks,rest) -> not(toks=[])) seqs)) in
            let tokens = remove_similar_dups nseqs in
            let stries = List.map (fun (tok) -> btt tok count seqs) tokens
            in
                stries@exps
    in
        (mode,bt 0 sequences)
    ;;

let usable_modes patterns s =
    Mylist.remove_dups (List.map
                    (fun (ParsePermit (m,s)) -> m)
                    (List.filter (fun x -> match x with | (ParsePermit (m,ss)) -> s=ss | _ -> false) patterns))
    ;;

let build_tries patterns =
    let modes = Mylist.remove_dups (List.fold_left List.append [] (List.map (fun x -> match x with | (ParsePermit (m,s)) -> [m] | _ -> []) patterns)) in
    let pats = List.map
                   (fun (ParsePattern (s,i,e,toks)) ->
                        List.map (fun (m) -> (ParsePattern (m,i,e,toks))) (usable_modes patterns s))
                   (List.filter (fun x -> match x with | (ParsePattern (s,i,e,toks)) -> true | _ -> false) patterns)
    in
        List.map (build_trie patterns) modes
    ;;

let print_sub sub =
    List.fold_right (fun x -> (fun y -> (x ^ " " ^ y)))
          (List.map (fun (x) -> ((Intern.decode x) ^ "->" ^ (prExp (Subst.apply sub x))))
              (Subst.dom sub)) "" ;;
let min a b = if a < b then a else b ;;

let rec find_min_prec t = match t with
  | (TrieResult (i,e,tl)) -> i
  | (TrieToken (t,tl)) ->
    find_min_prec_list tl
and find_min_prec_list l = match l with
  | [x] -> find_min_prec x
  | [] -> 100000
  | (a::b) -> min (find_min_prec a) (find_min_prec_list b) ;;

let rec strip_prec prec l = match l with
  | [] -> []
  | ((TrieResult (i,e,tl))::r) ->
    if i < prec then strip_prec prec r
    else (TrieResult (i,e,tl))::(strip_prec prec r)
  | ((TrieToken (t,tl))::r) ->
    if find_min_prec_list tl < prec then strip_prec prec r
    else (TrieToken (t,strip_prec prec tl))::(strip_prec prec r) ;;

let rec subterm_x n l = match l with
  | [] -> []
  | ((l,f,a,b,c,d)::r) -> (n::l,f,a,b,c,d)::(subterm_x n r)

let rec subterm_xlist n l = match l with
  | [] -> []
  | ((l,f,a,b,c,d)::r) -> (n@l,f,a,b,c,d)::(subterm_xlist n r) ;;

let rec subterm_xl n l = match l with
  | [] -> []
  | (l::r) -> (n@l)::(subterm_xl n r) ;;

let rec find_subterm v e = match e with
  | (VAR x) -> if v=x then [[]] else []
  | (APPL (f,l)) -> find_subterm_list v 0 l
  | _ -> []
and find_subterm_list v n l = match l with
  | [] -> []
  | (f::r) ->
    (subterm_xl [n] (find_subterm v f))@(find_subterm_list v (n+1) r) ;;

let rec generate_ind e indices =
    List.fold_left List.append []
        (List.map (fun (v,i) ->
             let fs = find_subterm v e
             in
                 List.fold_left List.append [] (List.map (fun (x) -> subterm_xlist x i) fs)
             ) indices)

let rec strip_tokens l = match l with
  | [] -> []
  | ((t,a,b,c,d,e)::r) -> t::(strip_tokens r) ;;

let progression = ref (10000,"",0,0) ;;

let reset_progression () =
    (progression := (10000,"",0,0)) ;;

exception ParseFailure of (string * int * int) ;;

let rec int_mk_failure l = match l with
  | ((t,f,r,c,_,_)::_) -> (f,r,c)
  | [] -> ("*END",0,0) ;;

let mk_failure l =
    let (count,pfile,pr,pc) = !progression
    in
        if count > List.length l then
            let (file,r,c) = int_mk_failure l
            in
                progression := (List.length l,file,r,c) ;
                (file,r,c)
        else
            (pfile,pr,pc)

let rec identifier_parse trie l = match l with
  | ((ID s,file,r1,c1,r2,c2)::r) -> (STRING s,r,[([],file,r1,c1,r2,c2)])
  | x -> raise (ParseFailure (mk_failure x))
and natural_parse trie l = match l with
  | ((NUMBER n,file,r1,c1,r2,c2)::r) -> (NUM n,r,[([],file,r1,c1,r2,c2)])
  | x -> raise (ParseFailure (mk_failure x))
and default_parse trie l = match l with
  | ((ID "ALL",f,rs,cs,_,_)::(SPECIAL "(",_,_,_,_,_)::r) ->
    let (vars,r1) = default_parse_vars trie r in
    let t = Parser.getTokenRc (SPECIAL ":") r1 in
    let (r2,_,_,_,_,_,_) = List.hd t in
    let (p,r3,l1) = parse trie "default" 0 r2 in
    let t = Parser.getTokenRc (SPECIAL ")") r3 in
    let (r4,_,_,_,_,_,_) = List.hd t in
    let (e,rest,l2) = parse trie "default" 0 r4 in
    let (_,_,_,_,re,ce) = List.hd l2 in
        ((QUANT (Intern.intern_all,vars,e,p)),rest,([],f,rs,cs,re,ce)::((subterm_x 0 l1)@(subterm_x 1 l2)))
  | ((ID "EXISTS",f,rs,cs,_,_)::(SPECIAL "(",_,_,_,_,_)::r) ->
    let (vars,r1) = default_parse_vars trie r in
    let t = Parser.getTokenRc (SPECIAL ")") r1 in
    let (r2,_,_,_,_,_,_) = List.hd t in
    let (e,rest,l) = parse trie "default" 0 r2 in
    let (_,_,_,_,re,ce) = List.hd l
    in
        ((QUANT (Intern.intern_exists,vars,e,(APPL (Intern.intern_true,[])))),rest,
         ([],f,rs,cs,re,ce)::(subterm_x 0 l))
  | ((ID s,f,rs,cs,_,_)::(SPECIAL "'",_,_,_,re,ce)::r) ->
    (MARKED_VAR (Intern.intern s), r, [([],f,rs,cs,re,ce)])
  | ((ID s,ff,rs,cs,_,_)::(SPECIAL "(",_,_,_,_,_)::r) ->
    let (f,r2,li) = parse trie "default" 0 r in
    let (l,r3,l2,re,ce) = default_parse_list 0 trie r2 in
        (APPL (Intern.intern s,f::l),r3,([],ff,rs,cs,re,ce)::((subterm_x 0 li)@l2))
  | ((ID s,f,rs,cs,re,ce)::r) ->
    (VAR (Intern.intern s), r,[([],f,rs,cs,re,ce)])
  | ((NUMBER n,f,rs,cs,re,ce)::r) ->
    (NUM n,r,[([],f,rs,cs,re,ce)])
  | ((QUOTE q,f,rs,cs,re,ce)::r) ->
    (STRING q,r,[([],f,rs,cs,re,ce)])
  | ((C_QUOTE q,f,rs,cs,re,ce)::r) ->
    (CHAR q,r,[([],f,rs,cs,re,ce)])
  | r -> raise (ParseFailure (mk_failure r))
and default_parse_list n trie l = match l with
  | ((SPECIAL ")",f,rs,cs,re,ce)::r) ->
    ([], r, [], re, ce)
  | ((SPECIAL ",",_,_,_,_,_)::r) ->
    let (t,r2,l) = parse trie "default" 0 r in
    let (tr,rest, l2, re, ce) = default_parse_list (n+1) trie r2
    in
        (t::tr, rest, (subterm_x n l)@l2, re ,ce)
  | l -> raise (ParseFailure (mk_failure l))
and default_parse_vars trie l = match l with
  | ((ID s,_,_,_,_,_)::(SPECIAL ",",_,_,_,_,_)::r) ->
    let (v2,r2) = default_parse_vars trie r
    in
         ((Intern.intern s, Type.notype)::v2,r2)
  | ((ID s,_,_,_,_,_)::r) ->
    ([(Intern.intern s, Type.notype)],r)
  | r -> raise (ParseFailure (mk_failure r))
and parse trie mode prec toks = trie_parse trie mode prec toks
and trie_parse trie mode prec l =
    let (*val _ = print ("tokens before (" ^ mode ^ ") " ^ (print_tokens2 l) ^ "\n")
        val _ = print_trie_set trie*)
        (exp1,rest1,indices) = parse_no_prefix trie mode prec l
        (*val _ = print ("tokens after " ^ (print_tokens2 rest1) ^ "\n")*)
    in
        let trl = get_trie mode trie in
        let rec find_mode_var l = match l with
              | [] -> []
              | ((TrieToken (TVAR (v,m,i),tl))::r) ->
                if m=mode then (v,i,tl)::(find_mode_var r) else find_mode_var r
              | (_::r) -> find_mode_var r in
        let trl = find_mode_var trl in
            (*val _ = print (foldr op^ "" (map (fn (v,i,tl) => ("Variable " ^ v ^ " " ^ (string_of_int i) ^ (foldr op^ "" (map (print_trie 4) tl)) ^ "\n")) trl))*)
        let xxx = if trl=[] then raise (ParseFailure (mk_failure l)) else () in
        let rec continue_rule tl prec rest sub indl =
                let tl = strip_prec prec tl in
                    (*val _ = print ("Continue rule " ^ (string_of_int prec) ^ "\n")
                let xxx = print ((foldr op^ "" (map (print_trie 4) tl)) ^ "\n")*)
                let rec cp l ll sub indl = match (l,ll) with
                      | (_,[]) -> raise (ParseFailure (mk_failure l))
                      | (l,((TrieToken (TVAR (v,m,i),tl))::r)) ->
                        if m=mode then
                           (sub,tl,v,i,l,NOEXP,indl)
                        else
                           (try let (exp1,rest,indices) = parse trie m i l
                            in
                                cp rest tl (Subst.addPair sub (Intern.intern v) exp1) ((Intern.intern v,indices)::indl)
                            with ParseFailure _ -> cp l r sub indl)
                      | (((f,a,b,c,d,e)::rest),((TrieToken (t,tl))::r)) ->
                        if f=t then cp rest tl sub indl else cp ((f,a,b,c,d,e)::rest) r sub indl
                      | (l,((TrieResult (i,e,tl))::r)) ->
                        (sub,[],"",i,l,e,indl)
                      | (l,_) -> raise (ParseFailure (mk_failure l))
                in
                    cp rest tl sub indl in
         let rec continue_parse exp indices exp_prec ret_prec rest sub =
                let xxx = if trl=[] then raise (ParseFailure (mk_failure rest)) else () in
                let (var,_,_) = List.hd trl in
                let trl = List.fold_right List.append
                              (List.map
                               (fun (_,_,r) -> r)
                              (List.filter
                               (fun (v,i,tl) -> i <= exp_prec) trl)) [] in
                    (*val _ = print "continue_parse\n"*)
                let (sub,tl,v,i,l,ex,indl) = continue_rule trl ret_prec rest sub [(Intern.intern var,indices)]
                    (*val _ = print ("var = " ^ var ^ "\n")*)
                in
                    (Subst.addPair sub (Intern.intern var) exp,tl,v,i,l,ex,indl) in
        try let rec decide_parse parent_prec ret_prec sub tl var toks indl =
                let (*val _ = print ("decide_parse " ^ var ^ " " ^ (print_tokens2 toks) ^ " " ^ (Int.toString parent_prec) ^ " " ^ (Int.toString ret_prec) ^ "\n")
                    xxx = print ("sub = " ^ (print_sub sub) ^ "\n")*)
                    (exp2,toks,indices) = parse_no_prefix trie mode ret_prec toks
                in
                    decide_parse1 parent_prec ret_prec sub tl var 10000 toks exp2 indices indl
            and decide_parse1 parent_prec ret_prec sub tl var exp_prec toks exp2 indices indl =
               (let (exp,rest,prec,indices) = initiate_parse exp2 toks indices ret_prec exp_prec in
                    (*val _ = print ("decide_parse0 " ^ (prExp exp) ^ " " ^ (print_tokens2 rest) ^ "\n")*)
                let sub =  Subst.addPair sub (Intern.intern var) exp in
                let indl = (Intern.intern var,indices)::indl in
                    (*val _ = print ("decide_parse1 " ^ (prExp exp) ^ " " ^ (print_tokens2 rest) ^ "\n")*)
                let (sub,tl,v,i,l,exp3,indl) = continue_rule tl parent_prec rest sub indl in
                    (*val _ = print ("decide_parse2 " ^ (prExp exp2) ^ " " ^ (prExp exp3) ^ " " ^ (Int.toString i) ^ " " ^ (print_tokens2 l) ^ "\n")*)
                let exp4 = Subst.subst sub exp3 in
                    if exp3=NOEXP then
                        decide_parse parent_prec ret_prec sub tl v l indl
                    else
                        (exp4,l,i,generate_ind exp3 indl)
                )
            and initiate_parse exp l indices ret_prec exp_prec =
               (try let (*val _ = print ("initiate2: " ^ (prExp exp) ^ " " ^ (print_tokens2 l) ^ " " ^ (Int.toString ret_prec) ^ " " ^ (Int.toString exp_prec) ^ "\n")*)
                    (sub,tl,v,p,l3,exp2,indices2) = continue_parse exp indices exp_prec ret_prec l Subst.empty in
                    (*val _ = print ("initiate3: " ^ (prExp exp2) ^ " " ^ (print_tokens2 l3) ^ " " ^ (Int.toString p) ^ "\n")*)
                let (e,r,prec,indices) =
                        if exp2=NOEXP then
                            decide_parse ret_prec p sub tl v l3 indices2
                        else
                            (Subst.subst sub exp2,l3,p,generate_ind exp2 indices2)
                in
                    initiate_parse e r indices ret_prec prec
                with ParseFailure (_) -> (exp,l,exp_prec,indices)) in
             let (e,r,p,indices) = initiate_parse exp1 rest1 indices 0 10000
        in
           (e,r,indices)
        with (ParseFailure _) -> (exp1,rest1,indices)
and parse_no_prefix trie mode prec toks = match mode with
  | "default" ->
    (try trie_parse_no_prefix trie "default" prec toks with ParseFailure (l) -> default_parse trie toks)
  | "natural" ->
    (try trie_parse_no_prefix trie "natural" prec toks with ParseFailure (l) -> natural_parse trie toks)
  | "identifier" ->
    (try trie_parse_no_prefix trie "identifier" prec toks with ParseFailure (l) -> identifier_parse trie toks)
  | mode -> trie_parse_no_prefix trie mode prec toks
and trie_parse_no_prefix trie mode prec l =
    let trl = get_trie mode trie in
    let trl2 = List.filter (fun t -> match t with | (TrieToken (TVAR (v,m,i),tr)) -> not(m=mode) | _ -> true) trl in
    let rec get_token t l = match l with
          | [] -> raise (ParseFailure (mk_failure []))
          | ((TrieToken(a,tr))::b) -> if t=a then tr else get_token t b
          | (_::b) -> get_token t b in
    let xxxx = if l=[] then raise (ParseFailure (mk_failure l)) else () in
        (*val tok = hd l*)
    let rest = l in
        (*val trie2 = get_token tok trl*)
    let rec get_results l = match l with
          | [] -> []
          | ((TrieResult (i,e,tl))::r) ->
            if i>=prec then e::(get_results r) else get_results r
          | (_::r) -> get_results r in
    let rec process_trie trie sub l indices re ce = match (trie,l) with
          | (_,[]) ->
            let  r = get_results trie
            in
                if r=[] then raise (ParseFailure ("*END",0,0))
                         else (Subst.subst sub (List.hd r),[],generate_ind (List.hd r) indices,re,ce)
          | ([],(f::r)) -> raise (ParseFailure(mk_failure (f::r)))
          | (((TrieToken (TVAR (v,m,i),t))::rest),(f::r)) ->
            (try let
                 (*val _ = print ("parsing var " ^ v ^ " " ^ m ^ " " ^ (Int.toString i) ^ "\n")*)
                 (exp,rest2,l) = parse [("",trie)] m i (f::r) in
             let (_,_,_,_,re,ce) = List.hd l
                 (*val _ = print "end parsing var\n"*)
             in
                 process_trie t (Subst.addPair sub (Intern.intern v) exp) rest2 ((Intern.intern v,l)::indices) re ce
             with ParseFailure l -> process_trie rest sub (f::r) indices re ce)
          | (((TrieToken (t,tr))::rest),((f,f1,rs1,cs1,re1,ce1)::r)) ->
            if t=f then
                process_trie tr sub r indices re1 ce1
            else
                process_trie rest sub ((f,f1,rs1,cs1,re1,ce1)::r) indices re ce
          | (((TrieResult (i,e,tl))::rest),l) ->
            if i>=prec then
                (Subst.subst sub e,l,generate_ind e indices,re,ce)
            else
                process_trie rest sub l indices re ce
          | ((_::rest),(f::r)) ->
            process_trie rest sub (f::r) indices re ce
          | (_,_) -> raise (ParseFailure (mk_failure l)) in
        (*val _ = print ("Processing " ^ (print_tokens2 rest) ^ "\n")*)
    let (_,f,rs,cs,re,ce) = List.hd rest in
    let (e,l,st,re,ce) = process_trie trl2 Subst.empty rest [] re ce
        (*val _ = print ("Result " ^ (print_tokens2 rest) ^ " " ^ (prExp e) ^ "\n")*)
    in
        (e,l,([],f,rs,cs,re,ce)::st)
    ;;

let ppParsePos (patterns,_) file s =
    let tokens = tokenize_rc file s in
    let patterns = filter_input patterns in
    let _ = reset_progression () in
        (*val _ = print "PATTERNS:\n"
        val _ = map (fn (x) => print ("  " ^ (print_directive x) ^ "\n")) patterns
        val _ = flush_out std_out*)
    let trie = build_tries patterns in
        (*val _ = print "TRIE:\n"
        val _ = print (print_trie_set trie)*)
    let (res,_,l) = parse trie "default" 0 tokens
    in
        (res,l)
    ;;

let ppParse p s =
    let (e,_) = ppParsePos p "input" s in e ;;

