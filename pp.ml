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
  | (OptionalBreak i) -> "optional(" ^ (Int.toString i) ^ ")"
  | (RequiredBreak i) -> "required(" ^ (Int.toString i) ^ ")"
  ;;

let rec print_break_list l1 l2 = match (l1,l2) with
  | ([],[]) -> ""
  | ([],(b::r)) ->
    (if b then "true " else "false ") ^ (print_break_list nil r)
  | ((a::r),[]) ->
    (print_break a) ^ " " ^ (print_break_list r nil)
  | ((a::r1),(b::r2)) ->
    (print_break a) ^ "-" ^ (if b then "true " else "false ") ^
    (print_break_list r1 r2)
  ;;

let prIlist l = match l with
  | [] -> ""
  | [a] -> Int.toString (a:int)
  | (a::b) -> (Int.toString a) ^ "," ^ (prIlist b)

let print_directive p = match p with
  | (ParsePattern (s,i,e,toks)) ->
    "Pattern " ^ s ^ " -> " ^ (Int.toString i) ^ " " ^ (prExp e) ^ ": " ^
    (print_tokens toks)
  | (PrintPattern (s,i,e,toks)) ->
    "PrintPattern " ^ s ^ " -> " ^ (Int.toString i) ^ " " ^ (prExp e) ^ ": " ^
    (print_tokens toks)
  | (InputPattern (s,i,e,toks)) ->
    "InputPattern " ^ s ^ " -> " ^ (Int.toString i) ^ " " ^ (prExp e) ^ ": " ^
    (print_tokens toks)
  | (ParsePermit (m,s)) ->
    "Mode " ^ m ^ " permits " ^ s
  | (ParseBreak (s,bl,bools)) ->
    "Breaks " ^ s ^ " -> " ^ (print_break_list bl bools)
  | (ParseMark (s,il,st,en)) ->
    "Mark(" ^ s ^ ",[" ^ (prIlist il) ^ "]," ^ (Int.toString st) ^ "," ^
    (Int.toString en) ^ ")"
  | _ -> "Other"
  ;;

let get_position sl = ((size (hd sl)),(length sl)-1)

let new_sl = [""]

let add_string (f::r) s = (f ^ s)::r

let add_return sl = ""::sl

let rec make_indent n = match n with
  | 0 -> ""
  | n -> " " ^ (make_indent (n-1))

let add_return_indent sl i = add_string (add_return sl) (make_indent i) ;;

let last_line_length (f::r) = size f ;;

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
    if f1=f2 && length l1=length l2 then
        has_match_list l1 l2
    else false
  | (_,_) -> false
and has_match_list l1 l2 = match (l1,l2) with
  | ([],[]) -> true
  | ((a::b),(c::d)) -> (has_match a c) && (has_match_list b d)
  | (_,_) -> false
  ;;

exception FailedMatch ;;

let rec u_match s pos p e = match ((VAR x),y) with
  | (x,pos,y)::s
  | ((APPL (f1,l1)),(APPL (f2,l2))) ->
    if f1=f2 && length l1 = length l2 then
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

let getExp l v = match l with
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

let split_pattern l = match l with
  | [] -> ([],[])
  | (TVAR ("","",0)::r) -> (nil,r)
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
          | [] -> empty
          | (a::b) -> addPair (ms b) a
                     (VAR (Intern.intern ((decode a) ^ "_" ^ (string_of_int n))))
    in
        subst (ms fv) e
    ;;

let rec token_replace_with_suffix l fv n = match l with
  | [] -> []
  | (TVAR (v,m,i)::r) ->
    TVAR(if member (intern v) fv then v ^ "_" ^ (Int.toString (n:int)) else v,m,i)::
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

let extend l n = match (l,n) with
  | ([a],0) -> []
  | ([a],n) -> if n<0 then nil else a::(extend [a] (n-1))
  | ((a::b),n) -> a::(extend b (n-1))
  ;;

let rec remove_empties l = match l with
  | [] -> []
  | ((TVAR ("","",i))::r) -> remove_empties r
  | (f::r) -> f::(remove_empties r)
  ;;

let stretch (APPL (f,l)) (ParsePattern (s,p,(APPL (f2,l2)),tl),breaks) =
    if length l=length l2 then
        (ParsePattern (s,p,(APPL (f2,l2)),remove_empties tl),breaks)
    else match(split_pattern tl) with
       | (_,[]) -> (ParsePattern (s,p,(APPL (f2,l2)),tl),breaks)
       | _ ->
                 (*val _ = print ("Stretching " ^ (prExp (APPL (f,l))) ^ " " ^ (prExp (APPL (f2,l2))) ^ " " ^ (print_tokens tl) ^ "\n")*)
        let (pre,mid) = split_pattern tl in
        let (middle,post) = split_pattern mid in
        let ll = last l2 in
        let fv = getFreeVars ll in
        let repeat_count = (length l)-(length l2)+1 in
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
        let b1 = length pre in
        let b2 = b1+(length middle) in
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
                                         not(member mode defaults) &&
                                         usable_in_mode ppenv mode s &&
                                         has_match e exp
                                  | _ -> false) get_directives ppenv) in
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
        if length p > 0 then
            [hd p]
        else if length patterns3 > 0 then
            [hd patterns3]
                else
            nil
    ;;

let p_cache = ref newExpItem (*: ((string * int * (directive * (((Break list) * (bool list)) list)) list) ExpDisc) ref*)

let get_patterns ppenv exp mode prec =
    let res = findExpItem (!p_cache) exp |> (fn (s,i,dl) => s=mode && i=prec)
    in
        if res=nil then
            let dl = int_get_patterns ppenv exp mode prec
            in
                 p_cache := addExpItem (!p_cache) exp (mode,prec,dl) ;
                 dl
        else
            let (s,i,dl) = (hd res) in dl
    ;;

let rec print_breaks b = match b with
  | [] -> ""
  | (ForbidBreak::r) -> "ForbidBreak " ^ (print_breaks r)
  | (OptionalBreak i::r) -> "OptionalBreak " ^ (Int.toString i) ^ " " ^ (print_breaks r)
  | (RequiredBreak i::r) -> "RequiredBreak " ^ (Int.toString i) ^ " " ^ (print_breaks r)
  ;;

let rec recPpExp ppenv exp wid one_line mode sl prefix markers indent failup prec force =
    (*let val patterns = get_directives ppenv
                     |> (fn (x) => case x of ParsePattern (s,p,e,tl) => prec < p && usable_in_mode ppenv mode s && has_match e exp | _ => false)*)
    let patterns = get_patterns ppenv exp mode prec in
        (*val _ = print ("exp = " ^ (prExp exp) ^ " " ^ (Int.toString indent) ^ "\n")
        val _ = print ("pattern length = " ^ (Int.toString (length patterns)) ^ "\n")*)
    let (start_x,start_y) = get_position sl in
    let rec pe l = match l with
          | [] -> (defaultPpProcess ppenv exp wid one_line mode sl prefix markers indent force
                      handle FailedPrint => if failup then raise FailedPrint else (add_return (add_string sl "*No parse*"),markers))
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
          | (a::b) -> try recPpProcessBreak ppenv exp wid one_line sl prefix markers indent (ParsePattern (s,prec,e,p)) a parse_marks (force && b=nil)
                            with FailedPrint -> rppb b
    in
        rppb (if one_line && length parse_breaks > 0 then [([ForbidBreak],[false])] else parse_breaks)
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
                       | (NUMBER n) -> (add_string_limit sl (Int.toString n) wid force,markers)
                       | (SPECIAL s) -> (add_string_limit sl s wid force,markers)
                       | (TVAR (v,m,prec)) -> recPpExp ppenv (getExp theta (intern v)) wid (blf || one_line) m sl ((getPos theta (intern v))@prefix) markers ind true prec force in
            let rec add_nl_tok sl marksers indent t force = match t with
                       | (ID s) -> (add_string_limit sl s wid force,markers)
                       | (SYMBOL s) -> (add_string_limit sl s wid force,markers)
                       | (NUMBER n) -> (add_string_limit sl (Int.toString n) wid force,markers)
                       | (SPECIAL " ") -> (sl,markers)
                       | (SPECIAL s) -> (add_string_limit sl s wid force,markers)
                       | (TVAR (v,m,prec)) -> recPpExp ppenv (getExp theta (intern v)) wid (blf || one_line) m sl ((getPos theta (intern v))@prefix) markers indent true prec force in
            let ((new_sl,new_markers),new_ind) =
                    if one_line then
                        (add_tok sl markers tf force,ind)
                    else match olf with
                           | ForbidBreak -> (add_tok sl markers tf force,ind)
                           | OptionalBreak(nn) -> (try (add_tok sl markers tf false,ind)
                                                   with FailedPrint -> ((add_nl_tok (add_return_indent sl (indent+nn)) markers (indent+nn) tf force),indent+nn))
                           | RequiredBreak(nn) -> (add_nl_tok (add_return_indent sl (indent+nn)) markers (indent+nn) tf force,indent+nn)
            in
                process new_sl new_holders new_markers (n+1) tr (next olf olr) (next blf blr) new_ind
    in
        process sl holders markers 0 p ol bl indent
and defaultPpProcess ppenv e wid one_line t sl prefix markers indent force = match (e,t) with
  | ((STRING s),"identifier") ->
    (add_string_limit sl s wid force, markers)
  | ((NUM n),"natural") ->
    (add_string_limit sl (Int.toString n) wid force, markers)
  | ((VAR x),"default") ->
    (add_string_limit sl (decode x) wid force, markers)
  | ((MARKED_VAR x),"default") ->
    (add_string_limit sl ((decode x) ^ "'") wid force, markers)
  | ((NUM n),"default") ->
    (add_string_limit sl (Int.toString n) wid force,
     markers)
  | ((STRING s),"default") ->
    (add_string_limit sl ("\"" ^ s ^ "\"") wid force,
     markers)
  | ((CHAR s),"default") ->
    (add_string_limit sl ("'" ^ (str s) ^ "'") wid force,
     markers)
  | ((APPL (f,l)),"default") ->
    (try let sl1 = add_string_limit sl ((decode f) ^ (if length l > 0 then "(" else "") ^ (if (length l)=0 && ((decode f) <"A" || (decode f) > "ZZZZ") then "()" else "")) wid force in
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
    else let sl1 = add_string_limit sl ((decode f) ^ (if length l > 0 then "(" else "") ^ (if length l=0 && (((decode f) < "A" ||  (decode f) > "ZZZ")) then "()" else "")) wid force in
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
    if Type.notType t then add_string_limit sl (decode v) wid force
                   else add_string_limit sl ((decode v) ^ ":" ^ (T.unparse t)) wid force
  | ((v,t)::r) ->
    varPpProcess r wid force
       (if Type.notType t then add_string_limit sl (Intern.decode v) wid force
        else add_string_limit sl ((Intern.decode v) ^ ":" ^ (Type.unparse t)) wid force)
  ;;

let rec filter_print l = match l with
  | [] -> []
  | ((InputPattern (t,i,e,l))::r) -> filter_print r
  | ((PrintPattern (t,i,e,l))::r) ->
    (ParsePattern (t,i,e,l))::(filter_print r)
  | (a::b) -> a::(filter_print b)
  ;;

let filter_input l = match l with
  | [] -> []
  | ((PrintPattern (t,i,e,l))::r) -> filter_input r
  | ((InputPattern (t,i,e,l))::r) ->
    (ParsePattern (t,i,e,l))::(filter_input r)
  | (a::b) -> a::(filter_input b)
  ;;

let ppExp ppenv exp wid =
    let xxx = p_cache := newExpItem in
    let (f,r) = ppenv in
    let ppenv = (filter_print f,r) in
    let (sl,markers) = recPpExp ppenv exp wid false "default" new_sl nil nil 0 false 0 true in
        (sl,map (fn (a,b,c,d,e) => (rev a,b,c,d,e)) markers)
    ;;

let ppOneLine ppenv exp =
    let (f,r) = ppenv in
    let ppenv = (filter_print f,r) in
    let (sl,markers) = recPpExp ppenv exp 10000 true "default" new_sl nil nil 0 false 0 true in
        (hd sl)
    ;;

let split t =
    let rec spl n f l = match l with
          | [] -> raise Failure t
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
                (rev f,r)
            else
                spl n ((SPECIAL ":")::f) r
          | (fr::r) -> spl n (fr::f) r
    in
        spl 0 nil t
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
          | t -> raise Failure t
    in
        pt r
  | x -> raise Failure x ;;

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
          | t -> raise Failure t in
        let (ol,bl) = pb ForbidBreak true r
    in
        ParseBreak (name,ol,bl)
  | x -> raise Failure x ;;

let parse_pattern tokens =
    let (name,prec,r) = match tokens with
                               | ((ID n)::(NUMBER i)::r) -> (n,i,r)
                               | ((ID n)::r)             -> (n,10000,r)
                               | _      -> raise Failure tokens in
    let (e_toks,p_toks) = split r in
    let _ = tokenParseExp e_toks in
        ParsePattern (name,prec,tokenParseExp e_toks,process_tokens p_toks) ;;

let rec parse_int_list l = match l with
  | [NUMBER n] -> [n]
  | ((NUMBER n)::(SPECIAL ",")::r) -> n::(parse_int_list r)
  | t -> raise Failure t
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
           | [(ID "permit"),(ID m),(ID r)] -> [ParsePermit (m,r)]
           | ((ID "break")::(ID name)::r)  -> [process_break name r]
           | ((ID "mark")::(ID name)::(NUMBER s)::(NUMBER e)::r)
                                           -> [ParseMark (name,parse_int_list r,s,e)]
           | _                             -> raise Failure t
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

fun print_trie i (TrieToken (t,tl)) =
    (indent i) ^ "Case " ^ (print_tokens [t]) ^ "\n" ^
    (foldr op^ "" (map (print_trie (i+2)) tl))
  | print_trie i (TrieResult (p,e,tl)) =
    (indent i) ^ "Result " ^ (Int.toString p) ^ " " ^ (prExp e) ^ " " ^
    (print_tokens tl) ^ "\n"

fun print_trie_set nil = ""
  | print_trie_set ((s,tl)::r) =
    "Mode: " ^ s ^ "\n" ^ (foldr op^ "" (map (print_trie 2) tl)) ^
    (print_trie_set r)

fun build_trie patterns mode =
    let val sequences = patterns <| (fn (ParsePattern (s,i,e,tl)) =>
                        if usable_in_mode (patterns,nil) mode s then
                            [(i,e,tl,nil)]
                        else nil | _ => nil)
        fun similar_token (TVAR (s1,s2,i1)) (TVAR (t1,t2,i2)) =
            s2=t2 && i1=i2
          | similar_token x y = x=y
        fun has_similar tok nil = false
          | has_similar tok (a::b) =
            (similar_token tok a) || (has_similar tok b)
        fun remove_similar_dups nil = nil
          | remove_similar_dups (a::b) =
            if has_similar a b then
                remove_similar_dups b
            else
                a::(remove_similar_dups b)
        fun rename_token n1 n2 (TVAR (s1,s2,prec)) =
            if s1 = n1 then
                (TVAR (n2,s2,prec))
            else
                (TVAR (s1,s2,prec))
          | rename_token n1 n2 x = x
        fun rename_var n1 n2 (i,e,tl,rest) =
            let val u = addPair empty (intern n1) (VAR (intern n2))
            in
                (i, subst u e, map (rename_token n1 n2) tl, rest)
            end
        fun rv count (i,e,(TVAR (s1,s2,prec)::r),rest) =
            rename_var s1 ("v" ^ (Int.toString count)) (i,e,(TVAR (s1,s2,prec)::r),rest)
          | rv count x = x
        fun make_name (TVAR (n,m,prec)) count = (TVAR (("v" ^ (Int.toString count)),m,prec))
          | make_name t _ = t
        fun ss nil = nil
          | ss (SPECIAL " "::r) = ss r
          | ss (a::b) = a::(ss b)
        fun strip_spaces (i,e,toks,rest) =
            (i,e,ss toks,rest)
        fun carry_ac_stuff (i,e,(TVAR ("","",0))::f::r,nil) =
            (i,e,f::r,[(TVAR ("","",0)),f])
          | carry_ac_stuff (i,e,(TVAR ("","",0))::r,x) =
            (i,e,nil,x@((TVAR ("","",0))::r))
          | carry_ac_stuff (i,e,x,nil) = (i,e,x,nil)
          | carry_ac_stuff (i,e,(f::r),x) = (i,e,(f::r),x@[f])
        fun btt token count seqs =
            let val seqs2 = seqs |> (fn (i,e,toks,rest) => (not(toks=nil) &&
                                     similar_token token (hd toks)))
                val seqs3 = seqs2 <> (rv count) <> (fn (i,e,toks,rest) => (i,e,tl toks,rest))
            in
                TrieToken (make_name token count,bt (count+1) seqs3)
            end
        and bt count seqs =
            let val seqs = seqs <> strip_spaces <> carry_ac_stuff
                val exps = seqs |> (fn (i,e,toks,rest) => toks=nil)
                                <> (fn (i,e,toks,rest) => TrieResult (i,e,rest))
                val nseqs = seqs |> (fn (i,e,toks,rest) => not(toks=nil))
                                 <> (fn (i,e,toks,rest) => (hd toks))
                val tokens = remove_similar_dups nseqs
                val stries = map (fn (tok) => btt tok count seqs) tokens
            in
                stries@exps
            end
    in
        (mode,bt 0 sequences)
    end

fun usable_modes patterns s =
    remove_dups (patterns |> (fn (ParsePermit (m,ss)) => s=ss | _ => false)
                          <> (fn (ParsePermit (m,s)) => m))

fun build_tries patterns =
    let val modes = remove_dups (patterns <| (fn (ParsePermit (m,s)) => [m] | _ => nil))
        val pats = patterns |> (fn (ParsePattern (s,i,e,toks)) => true | _ => false)
                            <> (fn (ParsePattern (s,i,e,toks)) =>
                                   map (fn (m) => (ParsePattern (m,i,e,toks))) (usable_modes patterns s))
    in
        map (build_trie patterns) modes
    end

fun print_sub sub =
    foldr (fn (x,y) => (x ^ " " ^ y))
          ""
          (map (fn (x) => ((decode x) ^ "->" ^ (prExp (apply sub x))))
              (dom sub))

fun find_min_prec (TrieResult (i,e,tl)) = i
  | find_min_prec (TrieToken (t,tl)) =
    find_min_prec_list tl
and find_min_prec_list [x] = find_min_prec x
  | find_min_prec_list nil = 100000
  | find_min_prec_list (a::b) =
    Int.min (find_min_prec a,find_min_prec_list b)

fun strip_prec prec nil = nil
  | strip_prec prec ((TrieResult (i,e,tl))::r) =
    if i < prec then strip_prec prec r
    else (TrieResult (i,e,tl))::(strip_prec prec r)
  | strip_prec prec ((TrieToken (t,tl))::r) =
    if find_min_prec_list tl < prec then strip_prec prec r
    else (TrieToken (t,strip_prec prec tl))::(strip_prec prec r)

fun subterm_x n nil = nil
  | subterm_x n ((l,f,a,b,c,d)::r) = (n::l,f,a,b,c,d)::(subterm_x n r)

fun subterm_xlist n nil = nil
  | subterm_xlist n ((l,f,a,b,c,d)::r) = (n@l,f,a,b,c,d)::(subterm_xlist n r)

fun subterm_xl n nil = nil
  | subterm_xl n (l::r) = (n@l)::(subterm_xl n r)

fun find_subterm v (VAR x) = if v=x then [nil] else nil
  | find_subterm v (APPL (f,l)) = find_subterm_list v 0 l
  | find_subterm v _ = nil
and find_subterm_list v n nil = nil
  | find_subterm_list v n (f::r) =
    (subterm_xl [n] (find_subterm v f))@(find_subterm_list v (n+1) r)

fun generate_ind e indices =
    foldl append nil
        (map (fn (v,i) =>
             let val fs = find_subterm v e
             in
                 foldl append nil (map (fn (x) => subterm_xlist x i) fs)
             end) indices)

fun strip_tokens nil = nil
  | strip_tokens ((t,a,b,c,d,e)::r) = t::(strip_tokens r)

val progression = ref (10000,"",0,0)

fun reset_progression () =
    (progression := (10000,"",0,0))

exception ParseFailure of string * int * int ;

fun int_mk_failure ((t,f,r,c,_,_)::_) = (f,r,c)
  | int_mk_failure nil = ("*END",0,0)

fun mk_failure l =
    let val (count,pfile,pr,pc) = !progression
    in
        if count > length l then
            let val (file,r,c) = int_mk_failure l
            in
                progression := (length l,file,r,c) ;
                (file,r,c)
            end
        else
            (pfile,pr,pc)
    end

fun identifier_parse trie ((ID s,file,r1,c1,r2,c2)::r) = (STRING s,r,[(nil,file,r1,c1,r2,c2)])
  | identifier_parse trie x = raise ParseFailure (mk_failure x)
and natural_parse trie ((NUMBER n,file,r1,c1,r2,c2)::r) = (NUM n,r,[(nil,file,r1,c1,r2,c2)])
  | natural_parse trie x = raise ParseFailure (mk_failure x)
and default_parse trie ((ID "ALL",f,rs,cs,_,_)::(SPECIAL "(",_,_,_,_,_)::r) =
    let val (vars,r1) = default_parse_vars trie r
        val t = getTokenRc (SPECIAL ":") r1
        val (r2,_,_,_,_,_,_) = hd t
        val (p,r3,l1) = parse trie "default" 0 r2
        val t = getTokenRc (SPECIAL ")") r3
        val (r4,_,_,_,_,_,_) = hd t
        val (e,rest,l2) = parse trie "default" 0 r4
        val (_,_,_,_,re,ce) = hd l2
    in
        ((QUANT (intern_all,vars,e,p)),rest,(nil,f,rs,cs,re,ce)::((subterm_x 0 l1)@(subterm_x 1 l2)))
    end
  | default_parse trie ((ID "EXISTS",f,rs,cs,_,_)::(SPECIAL "(",_,_,_,_,_)::r) =
    let val (vars,r1) = default_parse_vars trie r
        val t = getTokenRc (SPECIAL ")") r1
        val (r2,_,_,_,_,_,_) = hd t
        val (e,rest,l) = parse trie "default" 0 r2
                val (_,_,_,_,re,ce) = hd l
    in
        ((QUANT (intern_exists,vars,e,(APPL (intern_true,nil)))),rest,
         (nil,f,rs,cs,re,ce)::(subterm_x 0 l))
    end
  | default_parse trie ((ID s,f,rs,cs,_,_)::(SPECIAL "'",_,_,_,re,ce)::r) =
    (MARKED_VAR (intern s), r, [(nil,f,rs,cs,re,ce)])
  | default_parse trie ((ID s,ff,rs,cs,_,_)::(SPECIAL "(",_,_,_,_,_)::r) =
    let val (f,r2,li) = parse trie "default" 0 r
        val (l,r3,l2,re,ce) = default_parse_list 0 trie r2
    in
        (APPL (intern s,f::l),r3,(nil,ff,rs,cs,re,ce)::((subterm_x 0 li)@l2))
    end
  | default_parse trie ((ID s,f,rs,cs,re,ce)::r) =
    (VAR (intern s), r,[(nil,f,rs,cs,re,ce)])
  | default_parse trie ((NUMBER n,f,rs,cs,re,ce)::r) =
    (NUM n,r,[(nil,f,rs,cs,re,ce)])
  | default_parse trie ((QUOTE q,f,rs,cs,re,ce)::r) =
    (STRING q,r,[(nil,f,rs,cs,re,ce)])
  | default_parse trie ((C_QUOTE q,f,rs,cs,re,ce)::r) =
    (CHAR q,r,[(nil,f,rs,cs,re,ce)])
  | default_parse trie r = raise ParseFailure (mk_failure r)
and default_parse_list n trie ((SPECIAL ")",f,rs,cs,re,ce)::r) =
    (nil, r, nil, re, ce)
  | default_parse_list n trie ((SPECIAL ",",_,_,_,_,_)::r) =
    let val (t,r2,l) = parse trie "default" 0 r
        val (tr,rest, l2, re, ce) = default_parse_list (n+1) trie r2
    in
        (t::tr, rest, (subterm_x n l)@l2, re ,ce)
    end
  | default_parse_list _ _ l = raise ParseFailure (mk_failure l)
and default_parse_vars trie ((ID s,_,_,_,_,_)::(SPECIAL ",",_,_,_,_,_)::r) =
    let val (v2,r2) = default_parse_vars trie r
    in
         ((intern s, T.Notype)::v2,r2)
    end
  | default_parse_vars trie ((ID s,_,_,_,_,_)::r) =
    ([(intern s, T.Notype)],r)
  | default_parse_vars trie r = raise ParseFailure (mk_failure r)
and parse trie mode prec toks = trie_parse trie mode prec toks
and trie_parse trie mode prec l =
    let (*val _ = print ("tokens before (" ^ mode ^ ") " ^ (print_tokens2 l) ^ "\n")
        val _ = print_trie_set trie*)
        val (exp1,rest1,indices) = parse_no_prefix trie mode prec l
        (*val _ = print ("tokens after " ^ (print_tokens2 rest1) ^ "\n")*)
    in
        let val trl = get_trie mode trie
            fun find_mode_var nil = nil
              | find_mode_var ((TrieToken (TVAR (v,m,i),tl))::r) =
                if m=mode then (v,i,tl)::(find_mode_var r) else find_mode_var r
              | find_mode_var (_::r) = find_mode_var r
            val trl = find_mode_var trl
            (*val _ = print (foldr op^ "" (map (fn (v,i,tl) => ("Variable " ^ v ^ " " ^ (Int.toString i) ^ (foldr op^ "" (map (print_trie 4) tl)) ^ "\n")) trl))*)
            val _ = if trl=nil then raise ParseFailure (mk_failure l) else ()
            fun continue_rule tl prec rest sub indl =
                let val tl = strip_prec prec tl
                    (*val _ = print ("Continue rule " ^ (Int.toString prec) ^ "\n")
                    val _ = print ((foldr op^ "" (map (print_trie 4) tl)) ^ "\n")*)
                    fun cp l nil sub indl = raise ParseFailure (mk_failure l)
                      | cp l ((TrieToken (TVAR (v,m,i),tl))::r) sub indl =
                        if m=mode then
                           (sub,tl,v,i,l,NOEXP,indl)
                        else
                           (let val (exp1,rest,indices) = parse trie m i l
                            in
                                cp rest tl (addPair sub (intern v) exp1) ((intern v,indices)::indl)
                            end handle ParseFailure _ => cp l r sub indl)
                      | cp ((f,a,b,c,d,e)::rest) ((TrieToken (t,tl))::r) sub indl =
                        if f=t then cp rest tl sub indl else cp ((f,a,b,c,d,e)::rest) r sub indl
                      | cp l ((TrieResult (i,e,tl))::r) sub indl =
                        (sub,nil,"",i,l,e,indl)
                      | cp l _ _ _ = raise ParseFailure (mk_failure l)
                in
                    cp rest tl sub indl
                end
            fun continue_parse exp indices exp_prec ret_prec rest sub =
                let val _ = if trl=nil then raise ParseFailure (mk_failure rest) else ()
                    val (var,_,_) = hd trl
                    val trl = foldr append
                              nil
                              (trl |>
                               (fn (v,i,tl) => i <= exp_prec) <>
                               (fn (_,_,r) => r))
                    (*val _ = print "continue_parse\n"*)
                    val (sub,tl,v,i,l,ex,indl) = continue_rule trl ret_prec rest sub [(intern var,indices)]
                    (*val _ = print ("var = " ^ var ^ "\n")*)
                in
                    (addPair sub (intern var) exp,tl,v,i,l,ex,indl)
                end
            fun decide_parse parent_prec ret_prec sub tl var toks indl =
                let (*val _ = print ("decide_parse " ^ var ^ " " ^ (print_tokens2 toks) ^ " " ^ (Int.toString parent_prec) ^ " " ^ (Int.toString ret_prec) ^ "\n")
                    val _ = print ("sub = " ^ (print_sub sub) ^ "\n")*)
                    val (exp2,toks,indices) = parse_no_prefix trie mode ret_prec toks
                in
                    decide_parse1 parent_prec ret_prec sub tl var 10000 toks exp2 indices indl
                end
            and decide_parse1 parent_prec ret_prec sub tl var exp_prec toks exp2 indices indl =
               (let val (exp,rest,prec,indices) = initiate_parse exp2 toks indices ret_prec exp_prec
                    (*val _ = print ("decide_parse0 " ^ (prExp exp) ^ " " ^ (print_tokens2 rest) ^ "\n")*)
                    val sub =  addPair sub (intern var) exp
                    val indl = (intern var,indices)::indl
                    (*val _ = print ("decide_parse1 " ^ (prExp exp) ^ " " ^ (print_tokens2 rest) ^ "\n")*)
                    val (sub,tl,v,i,l,exp3,indl) = continue_rule tl parent_prec rest sub indl
                    (*val _ = print ("decide_parse2 " ^ (prExp exp2) ^ " " ^ (prExp exp3) ^ " " ^ (Int.toString i) ^ " " ^ (print_tokens2 l) ^ "\n")*)
                    val exp4 = subst sub exp3
                in
                    if exp3=NOEXP then
                        decide_parse parent_prec ret_prec sub tl v l indl
                    else
                        (exp4,l,i,generate_ind exp3 indl)
                end)
            and initiate_parse exp l indices ret_prec exp_prec =
               (let (*val _ = print ("initiate2: " ^ (prExp exp) ^ " " ^ (print_tokens2 l) ^ " " ^ (Int.toString ret_prec) ^ " " ^ (Int.toString exp_prec) ^ "\n")*)
                    val (sub,tl,v,p,l3,exp2,indices2) = continue_parse exp indices exp_prec ret_prec l empty
                    (*val _ = print ("initiate3: " ^ (prExp exp2) ^ " " ^ (print_tokens2 l3) ^ " " ^ (Int.toString p) ^ "\n")*)
                    val (e,r,prec,indices) =
                        if exp2=NOEXP then
                            decide_parse ret_prec p sub tl v l3 indices2
                        else
                            (subst sub exp2,l3,p,generate_ind exp2 indices2)
                in
                    initiate_parse e r indices ret_prec prec
                end handle ParseFailure (_) => (exp,l,exp_prec,indices))
             val (e,r,p,indices) = initiate_parse exp1 rest1 indices 0 10000
        in
           (e,r,indices)
        end handle ParseFailure _ => (exp1,rest1,indices)
    end
and parse_no_prefix trie "default" prec toks =
    (trie_parse_no_prefix trie "default" prec toks handle ParseFailure (l) => default_parse trie toks)
  | parse_no_prefix trie "natural" prec toks =
    (trie_parse_no_prefix trie "natural" prec toks handle ParseFailure (l) => natural_parse trie toks)
  | parse_no_prefix trie "identifier" prec toks =
    (trie_parse_no_prefix trie "identifier" prec toks handle ParseFailure (l) => identifier_parse trie toks)
  | parse_no_prefix trie mode prec toks = trie_parse_no_prefix trie mode prec toks
and trie_parse_no_prefix trie mode prec l =
    let val trl = get_trie mode trie
        val trl2 = trl |> (fn (TrieToken (TVAR (v,m,i),tr)) => not(m=mode) | _ => true)
        fun get_token t nil = raise ParseFailure (mk_failure l)
          | get_token t ((TrieToken(a,tr))::b) = if t=a then tr else get_token t b
          | get_token t (_::b) = get_token t b
        val _ = if l=nil then raise ParseFailure (mk_failure l) else ()
        (*val tok = hd l*)
        val rest = l
        (*val trie2 = get_token tok trl*)
        fun get_results nil = nil
          | get_results ((TrieResult (i,e,tl))::r) =
            if i>=prec then e::(get_results r) else get_results r
          | get_results (_::r) = get_results r
        fun process_trie trie sub nil indices re ce =
            let val r = get_results trie
            in
                if r=nil then raise ParseFailure ("*END",0,0)
                         else (subst sub (hd r),nil,generate_ind (hd r) indices,re,ce)
            end
          | process_trie nil sub (f::r) indices re ce = raise ParseFailure(mk_failure (f::r))
          | process_trie ((TrieToken (TVAR (v,m,i),t))::rest) sub (f::r) indices re ce =
            (let
                 (*val _ = print ("parsing var " ^ v ^ " " ^ m ^ " " ^ (Int.toString i) ^ "\n")*)
                 val (exp,rest2,l) = parse trie m i (f::r)
                 val (_,_,_,_,re,ce) = hd l
                 (*val _ = print "end parsing var\n"*)
             in
                 process_trie t (addPair sub (intern v) exp) rest2 ((intern v,l)::indices) re ce
             end handle ParseFailure l => process_trie rest sub (f::r) indices re ce)
          | process_trie ((TrieToken (t,tr))::rest) sub ((f,f1,rs1,cs1,re1,ce1)::r) indices re ce =
            if t=f then
                process_trie tr sub r indices re1 ce1
            else
                process_trie rest sub ((f,f1,rs1,cs1,re1,ce1)::r) indices re ce
          | process_trie ((TrieResult (i,e,tl))::rest) sub l indices re ce =
            if i>=prec then
                (subst sub e,l,generate_ind e indices,re,ce)
            else
                process_trie rest sub l indices re ce
          | process_trie (_::rest) sub (f::r) indices re ce =
            process_trie rest sub (f::r) indices re ce
          | process_trie _ _ l indices re ce = raise ParseFailure (mk_failure l)
        (*val _ = print ("Processing " ^ (print_tokens2 rest) ^ "\n")*)
        val (_,f,rs,cs,re,ce) = hd rest
        val (e,l,st,re,ce) = process_trie trl2 empty rest nil re ce
        (*val _ = print ("Result " ^ (print_tokens2 rest) ^ " " ^ (prExp e) ^ "\n")*)
    in
        (e,l,(nil,f,rs,cs,re,ce)::st)
    end

fun ppParsePos (patterns,_) file s =
    let val tokens = tokenize_rc file s
        val patterns = filter_input patterns
        val _ = reset_progression ()
        (*val _ = print "PATTERNS:\n"
        val _ = map (fn (x) => print ("  " ^ (print_directive x) ^ "\n")) patterns
        val _ = flush_out std_out*)
        val trie = build_tries patterns
        (*val _ = print "TRIE:\n"
        val _ = print (print_trie_set trie)*)
        val (res,_,l) = parse trie "default" 0 tokens
    in
        (res,l)
    end

fun ppParse p s =
    let val (e,_) = ppParsePos p "input" s in e end

