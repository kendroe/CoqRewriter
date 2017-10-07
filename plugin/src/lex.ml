(******************************************************************************
 *                                   
 * REWRITELIB                        
 *                                   
 * lex.ml
 *                                   
 * Implementation of lexical analysis module
 *                                   
 * (C) 2017, Kenneth Roe             
 *                                   
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * will be provided when the work is complete.
 *                                   
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com     
 *                                   
 *****************************************************************************)
  
(* require "list.sml" ;  *)
(* require "intern.sml" ;  *)
(* require "getfile.sml" ;  *)
(* require "lex-s.sml" ;  *)
(* require "basis.__integer" ;  *)

open Getfile;;

type token = ID of string
           | SYMBOL of string
           | QUOTE of string
           | C_QUOTE of char
           | NUMBER of int
           | RAT of int * int
           | SPECIAL of string
           | TVAR of string * string * int ;;

exception Failure of token list ;;

let rec print_tokens tl = match tl with
  | [] -> ""
  | ((ID s)::r) -> "ID(" ^ s ^")" ^ (print_tokens r)
  | ((SYMBOL s)::r) -> "SYMBOL(" ^ s ^")" ^ (print_tokens r)
  | ((QUOTE s)::r) -> "QUOTE(" ^ s ^")" ^ (print_tokens r)
  | ((C_QUOTE s)::r) -> "C_QUOTE(" ^ (String.make 1 s) ^")" ^ (print_tokens r)
  | ((NUMBER i)::r) -> "NUMBER(" ^ (string_of_int i) ^ ")" ^ (print_tokens r)
  | ((RAT (i,j))::r) -> "RAT(" ^ (string_of_int i) ^ "/" ^ (string_of_int j) ^ ")" ^ (print_tokens r)
  | ((SPECIAL s)::r) -> "SPECIAL(" ^ s ^")" ^ (print_tokens r)
  | ((TVAR (a,b,i))::r) -> "TVAR(" ^ a ^ "," ^ b ^ "," ^ (string_of_int i) ^")" ^ (print_tokens r) ;;

let rec print_tokens2 tl = match tl with
  | [] -> ""
  | ((ID s,f,r1,c1,r2,c2)::r) -> "ID(" ^ s ^ ")[" ^
                  f ^ "," ^ (string_of_int r1) ^ "," ^ (string_of_int c1) ^ "," ^ (string_of_int r2) ^ "," ^ (string_of_int c2) ^ "]" ^
                  (print_tokens2 r)
  | ((SYMBOL s,f,r1,c1,r2,c2)::r) -> "SYMBOL(" ^ s ^ ")[" ^
                  f ^ "," ^ (string_of_int r1) ^ "," ^ (string_of_int c1) ^ "," ^ (string_of_int r2) ^ "," ^ (string_of_int c2) ^ "]" ^
                  (print_tokens2 r)
  | ((QUOTE s,f,r1,c1,r2,c2)::r) -> "QUOTE(" ^ s ^")[" ^
                  f ^ "," ^ (string_of_int r1) ^ "," ^ (string_of_int c1) ^ "," ^ (string_of_int r2) ^ "," ^ (string_of_int c2) ^ "]" ^
                  (print_tokens2 r)
  | ((NUMBER i,f,r1,c1,r2,c2)::r) -> "NUMBER(" ^ (string_of_int i) ^")[" ^
                  f ^ "," ^ (string_of_int r1) ^ "," ^ (string_of_int c1) ^ "," ^ (string_of_int r2) ^ "," ^ (string_of_int c2) ^ "]" ^
          (print_tokens2 r)
  | ((RAT (i,j),f,r1,c1,r2,c2)::r) -> "RAT(" ^ (string_of_int i) ^ "/" ^ (string_of_int j) ^ ")[" ^
                  f ^ "," ^ (string_of_int r1) ^ "," ^ (string_of_int c1) ^ "," ^ (string_of_int r2) ^ "," ^ (string_of_int c2) ^ "]" ^
                  (print_tokens2 r)
  | ((SPECIAL s,f,r1,c1,r2,c2)::r) -> "SPECIAL(" ^ s ^")" ^
                  f ^ "," ^ (string_of_int r1) ^ "," ^ (string_of_int c1) ^ "," ^ (string_of_int r2) ^ "," ^ (string_of_int c2) ^ "]" ^
                  (print_tokens2 r)
  | ((TVAR (a,b,i),f,r1,c1,r2,c2)::r) -> "TVAR(" ^ a ^ "," ^ b ^ "," ^ (string_of_int i) ^")[" ^
                  f ^ "," ^ (string_of_int r1) ^ "," ^ (string_of_int c1) ^ "," ^ (string_of_int r2) ^ "," ^ (string_of_int c2) ^ "]" ^
                  (print_tokens2 r) ;;

let digit x = x >= '0' && x <= '9' ;;

let alpha x = (x >= 'a' && x <= 'z') ||
              (x >= 'A' && x <= 'Z') ||
              (x = '_') ;;

let rec tok file l row col = match l with
  | [] -> []
  | (' '::r) -> tok file r row (col+1)
  | ('/':: '/':: '#'::r) -> (SPECIAL "//#",file,row,col,row,col+2)::(tok file r row (col+3))
  | ('/':: '*':: '#'::r) -> (SPECIAL "/*#",file,row,col,row,col+2)::(tok file r row (col+3))
  | ('/':: '/'::r) -> comment1 file r row col
  | ('/':: '*'::r) -> comment2 file r row col 1
  | ('\n':: '#'::r) -> new_file_line "" r
  | ('\n'::r) -> tok file r (row+1) 0
  | ('('::r) -> (SPECIAL "(",file,row,col,row,col) :: tok file r row (col + 1)
  | (')'::r) -> (SPECIAL ")",file,row,col,row,col) :: tok file r row (col + 1)
  | ('['::r) -> (SPECIAL "[",file,row,col,row,col) :: tok file r row (col + 1)
  | (']'::r) -> (SPECIAL "]",file,row,col,row,col) :: tok file r row (col + 1)
  | ('{'::r) -> (SPECIAL "{",file,row,col,row,col) :: tok file r row (col + 1)
  | ('}'::r) -> (SPECIAL "}",file,row,col,row,col) :: tok file r row (col + 1)
  | (':'::r) -> (SPECIAL ":",file,row,col,row,col) :: tok file r row (col + 1)
  | (','::r) -> (SPECIAL ",",file,row,col,row,col) :: tok file r row (col + 1)
  | ('\'':: '\\'::r) -> qbparse file r row (col + 2)
  | ('\''::c:: '\''::r) -> (C_QUOTE c,file,row,col,row,col+2)::(tok file r row (col + 3))
  | ('\''::r) -> (SPECIAL "'",file,row,col,row,col) :: tok file r row (col + 1)
  | ('\\':: 'b'::r) -> (SPECIAL " ",file,row,col,row,col+1) :: tok file r row (col+2)
  | (';'::r) -> (SPECIAL ";",file,row,col,row,col) :: tok file r row (col+1)
  | ('\"'::r) -> qtok file "" '\"' r row col row col
  | (x::r) -> if digit x then
                  tokenizeInt file ((int_of_char x) - (int_of_char '0')) r row col row (col+1)
              else (if alpha x then
                  tokenizeAlpha file (String.make 1 x) r row col row (col+1)
              else
                  tokenizeSymbol file (String.make 1 x) r row col row (col+1))
and comment1 file t row col = match t with
  | ('\n'::r) -> tok file r (row+1) 0
  | (x::r) -> comment1 file r row (col+1)
and comment2 file r row col n = match (r,n) with
  | (r,0) -> tok file r row col
  | (('/':: '*' ::r),n) -> comment2 file r row (col+2) (n+1)
  | (('*' :: '/'::r),n) -> comment2 file r row (col+2) (n-1)
  | (('\n'::r),n) -> comment2 file r (row+1) 0 n
  | ((_::r),n) -> comment2 file r row (col+1) n
and new_file_line s tl = match tl with
  | ('\n'::r) ->
    (let line = (int_of_string (before_space (after_space s))) in
     let file = before_space (after_space (after_space s)) in
     let file = String.sub file 1 ((String.length file)-2) in
        tok file r line 0)
  | (l::r) -> new_file_line (s ^ (String.make 1 l)) r
and qbparse file (a::b::c::r) row col =
    if a >= '0' && a <= '9' then
       (if b >= '0' && b <= '9' then
           (if c >= '0' && c <= '9' then
                (C_QUOTE (char_of_int (((int_of_char a)-int_of_char('0'))*100+((int_of_char b)-(int_of_char '0'))*10+((int_of_char c)-(int_of_char '0')))),file,row,col-2,row,col+3)::(skip_quote file r row (col+3))
            else
                (C_QUOTE (char_of_int (((int_of_char a)-int_of_char('0'))*10+((int_of_char b)-(int_of_char '0')))),file,row,col-2,row,col+2)::(skip_quote file (c::r) row (col + 2)))
        else
            (C_QUOTE (char_of_int ((int_of_char a)-int_of_char('0'))),file,row,col-2,row,col+1)::(skip_quote file (b::c::r) row (col+1)))
    else
        (C_QUOTE
            (if a= 'n' then '\n' else if a= 't' then '\t' else a),file,row,col-2,row,col+1
        )::(skip_quote file (b::c::r) row (col+1))
and skip_quote file t row col = match t with
  | ('\''::r) -> tok file r row (col+1)
  | r -> tok file r row col
and qtok file s eq tl srow scol row col = match tl with
  | ('\\'::a::b::c::r) ->
    if a >= '0' && a <= '9' then
       (if b >= '0' && b <= '9' then
           (if c >= '0' && c <= '9' then
                qtok file (s ^ (String.make 1 (char_of_int (((int_of_char a)-int_of_char('0'))*100+((int_of_char b)-(int_of_char '0'))*10+((int_of_char c)-(int_of_char '0')))))) eq r srow scol row (col+3)
            else
                qtok file (s ^ (String.make 1 (char_of_int (((int_of_char a)-int_of_char('0'))*10+((int_of_char b)-(int_of_char '0')))))) eq (c::r) srow scol row (col+2))
        else
            qtok file (s ^ (String.make 1 (char_of_int ((int_of_char a)-int_of_char('0'))))) eq (b::c::r) srow scol row (col+1))
    else
        qtok file (s ^ (String.make 1 (if a= 'n' then '\n' else if a='t' then '\t' else a))) eq (b::c::r) srow scol row (col+1)
  | (f::r) ->
    if f=eq then ((QUOTE s),file,srow,scol,row,col)::(tok file r row (col+1)) else qtok file (s ^ (String.make 1 f)) eq r srow scol row (col+1)
  | [] -> [(QUOTE s,file,srow,scol,row,col)]
and tokenizeInt file v tl srow scol row col = match tl with
  | [] -> [(NUMBER v,file,srow,scol,row,col)]
  | (a::b) -> if digit a then
                  tokenizeInt file (v * 10 + ((int_of_char a) - (int_of_char '0')))
                             b srow scol row (col+1)
              else if a= '#' then
                  startTokenizeNumber file v 0 b srow scol row (col+1)
              else ((NUMBER v),file,srow,scol,row,col-1)::(tok file (a::b) row col)
and startTokenizeNumber file n v tl srow scol row col = match tl with
  | (a::b) -> if digit a then tokenizeNumber file n v (a::b) srow scol row col
    else (NUMBER n,file,srow,scol,row,col-2)::(tok file ('#'::a::b) row (col-1))
  | [] -> (NUMBER n,file,srow,scol,row,col-2)::(tok file ['#'] row (col-1))
and tokenizeNumber file n v tl srow scol row col = match tl with
  | [] -> [(RAT (n,v),file,srow,scol,row,col)]
  | (a::b) -> if digit a then
                  tokenizeNumber file n (v * 10 + ((int_of_char a) - (int_of_char '0')))
                                       b srow scol row (col+1)
              else ((RAT (n,v)),file,srow,scol,row,col-1)::(tok file (a::b) row col)
and tokenizeAlpha file v tl srow scol row col = match tl with
  | [] -> [(ID v,file,srow,scol,row,col)]
  | (a::b) -> if (alpha a) || (digit a) then
                  tokenizeAlpha file (v ^ (String.make 1 a)) b srow scol row (col+1)
              else
                  (ID v,file,srow,scol,row,col-1)::(tok file (a::b) row col)
and tokenizeSymbol file v tl srow scol row col = match tl with
  | [] -> [(SYMBOL v,file,srow,scol,row,col)]
  | (a::b) -> if (digit a) || (alpha a) ||
                 a = ' ' || a = '(' || a = '{'
                 || a = '[' || a = ',' || a = ' ' ||
                   a = ':' || a = '\n' then
                  (SYMBOL v,file,srow,scol,row,col-1)::(tok file (a::b) row col)
              else
                  tokenizeSymbol file (v ^ (String.make 1 a)) b srow scol row (col+1) ;;

let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) [];;

let tokenize_rc f s = tok f (explode s) 0 0 ;;

let tokenize s = List.map (fun (a,b,c,d,e,f) -> a) (tokenize_rc "" s) ;;





