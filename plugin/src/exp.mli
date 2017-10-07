(******************************************************************************
 *
 * REWRITELIB
 *
 * exp.mli
 *
 * This file contains signatures and global data types.
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

(* require "type-s.sml" ;  *)
(* require "lex.sml" ;  *)

type exp = VAR of int
         | MARKED_VAR of int
         | QUANT of (int * (int * Rtype.etype) list * exp * exp)
         | APPL of (int * exp list)
         | LET of (exp * Rtype.etype * exp *exp)
         | CASE of exp * Rtype.etype * ((exp * exp) list)
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
exception BadParse of int ;;
val getName: exp -> int ;;
val getArgs: exp -> exp list ;;
val allSymbols: exp -> (int list) ;;
(*
 * Subterm retrieval
 *)
exception BadSubterm ;;
val getSubterm : exp -> (int list) -> exp ;;
val replaceSubterm : exp -> (int list) -> exp ->exp ;;
val subtermCount : exp -> int ;;
val allSubterms : exp -> (int list list) ;;
val remove_normals : exp -> exp ;;
(*
 * Printing/parsing
 *)
val prExp: exp -> string ;;
val parseExp: string -> exp ;;
val tokenParseExp: (Lex.token list) -> exp ;;
val parseRule: string -> exp ;;
val tokenParseRule: (Lex.token list) -> exp ;;
val parseRuleList: string -> exp list ;;

