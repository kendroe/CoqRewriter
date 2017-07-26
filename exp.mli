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
 * This file is distributed under the GNU General Public License (version 3).
 * See the file LICENSE.txt for details.
 *
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *
 *****************************************************************************)

(* require "type-s.sml" ;  *)
(* require "lex.sml" ;  *)

type exp = VAR of int
         | MARKED_VAR of int
         | QUANT of (int * (int * Type.etype) list * exp * exp)
         | APPL of (int * exp list)
         | LET of (exp * Type.etype * exp *exp)
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



