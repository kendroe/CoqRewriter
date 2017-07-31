(******************************************************************************
 *
 * REWRITELIB
 *
 * pp.mli
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




