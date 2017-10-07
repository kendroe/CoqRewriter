(******************************************************************************
 *
 * REWRITELIB
 *
 * prettyPrint.mli
 *
 * Pretty printing
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

(* require "lex.sml" ;  *)
(* require "exp.sml" ;  *)

type ppenv ;;

type precedence = LEFT of Lex.token * int | RIGHT of Lex.token * int ;;

type break = ForbidBreak
           | OptionalBreak of int
           | RequiredBreak of int
           ;;

type directive = ParsePattern of string * int * Exp.exp * (Lex.token list)
               | PrintPattern of string * int * Exp.exp * (Lex.token list)
               | InputPattern of string * int * Exp.exp * (Lex.token list)
               | ParsePermit of string * string
               | ParseBreak of string * (break list) * (bool list)
               | ParsePrecedence of string * precedence
               | ParseMark of string * (int list) * int * int
               ;;

val emptyPpenv : ppenv ;;

val addDirective : ppenv -> directive -> ppenv ;;

val parseDirective : string -> (directive list) ;;

val addParseDirectives : ppenv -> string -> ppenv ;;

val ppExp : ppenv -> Exp.exp -> int ->
            (string list * ((int list * int * int * int * int) list)) ;;

exception ParseFailure of (string * int * int) ;;

val ppOneLine : ppenv -> Exp.exp -> string ;;

val ppParsePos : ppenv -> string -> string -> (Exp.exp * (((int list) * string * int * int * int * int) list)) ;;
val ppParse : ppenv -> string -> Exp.exp ;;




