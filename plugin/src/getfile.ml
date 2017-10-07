(******************************************************************************
 *                                   
 * REWRITELIB                        
 *                                   
 * getfile.ml                        
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

(* require "getfil-s.sml" ;  *)
(* require "basis.__text_io" ;  *)

let rec remove_leaders s =
    if s = "" then
        ""
    else
        if (String.get s 0 = ' ') then
            remove_leaders (String.sub s 1 ((String.length s) - 1))
        else
            s ;;

let is_prefix a b = try (String.sub b 0 (String.length a) = a)
                    with Invalid_argument q -> false ;;

let rec before_space s = match s with
  | "" -> ""
  | s -> 
         if (String.get s 0) = ' ' then
             ""
         else
             (String.make 1 (String.get s 0)) ^
             (before_space (String.sub s 1 ((String.length s)-1))) ;;

let rec after_space s = match s with
  | "" -> ""
  | s -> if (String.get s 0 = ' ') then remove_leaders s
         else (after_space (String.sub s 1 ((String.length s) - 1))) ;;

let rec before_equal s = match s with
  | "" -> ""
  | s -> 
    if (String.get s 0) = '=' then
        ""
    else
        (String.sub s 0 1) ^
        (before_equal (String.sub s 1 ((String.length s)-1))) ;;

let rec after_equal s = match s with
  | "" -> ""
  | s -> if (String.get s 0 = '=') then
             remove_leaders (String.sub s 1 ((String.length s) - 1))
         else (after_equal (String.sub s 1 ((String.length s) - 1))) ;;

let rec before_match_paren n s split = match s with
  | "" -> ""
  | s ->
    if (String.get s 0) = '(' then
        "(" ^ (before_match_paren (n+1) (String.sub s 1 ((String.length s)-1)) split)
    else if (String.get s 0) = ')' then
        ")" ^ (if n=0 then
                   split (String.sub s 1 ((String.length s)-1))
               else
                  (before_match_paren (n-1) (String.sub s 1 ((String.length s)-1)) split))
    else
        (String.sub s 0 1) ^
        (before_match_paren n (String.sub s 1 ((String.length s)-1)) split) ;;

let rec after_match_paren n s split = match s with
  | "" -> ""
  | s ->
    if (String.get s 0) = '(' then
        (after_match_paren (n+1) (String.sub s 1 ((String.length s)-1)) split)
    else if (String.get s 0) = ')' then
        (if n=0 then
             split (String.sub s 1 ((String.length s)-1))
         else
             (after_match_paren (n-1) (String.sub s 1 ((String.length s)-1)) split))
    else
        (after_match_paren n (String.sub s 1 ((String.length s)-1)) split) ;;

let rec before_comma s = match s with
  | "" -> ""
  | s ->
    if (String.get s 0) = ',' then
        ""
    else if (String.get s 0) = '(' then
        "(" ^ (before_match_paren 0 (String.sub s 1 ((String.length s)-1)) before_comma)
    else
        (String.sub s 0 1) ^
        (before_comma (String.sub s 1 ((String.length s)-1))) ;;

let rec after_comma s = match s with
  | "" -> ""
  | s ->
    if (String.get s 0) = ',' then
         remove_leaders (String.sub s 1 ((String.length s) - 1))
    else if (String.get s 0 = '(') then
         after_match_paren 0 (String.sub s 1 ((String.length s) - 1)) after_comma
    else (after_comma (String.sub s 1 ((String.length s) - 1))) ;;

let rec di x l = match l with
  | [] -> x
  | (a::b) -> if a >= '0' && a <= '9' then
                      di ((int_of_char a) - (int_of_char '0') + (x * 10)) b
                  else
                      di x b
  ;;

let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) [];;

let decode_integer s = di 0 (explode s) ;;

let getcline f =
    (let l1 = input_line f
    in
        (String.sub l1 0 ((String.length l1) - 1)))
    ;;

let rec getline f =
    try 
        let ll = input_line f in
        let l = (String.sub ll 0 ((String.length ll) - 1))
        in
            if String.length l = 0 then
                getline f
            else if String.get l 0 = '#' then
                getline f else l
    with End_of_file -> "" ;;

let rec getfile2 f =
    try
        let v = getline f in
            v::getfile2 f
    with End_of_file -> (close_in f;[]) ;;

let getfile s = getfile2 (open_in s) ;;

let rec getcfile2 f =
    try
        let v = getcline f in
            v::getcfile2 f
    with End_of_file -> (close_in f;[]);;

let getcfile s = getcfile2 (open_in s) ;;

let rec append_text l = match l with
  | [] -> ""
  | (a::b) -> a ^ "\n" ^ (append_text b);;

