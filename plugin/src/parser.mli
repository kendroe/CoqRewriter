(******************************************************************************
 *
 * REWRITELIB
 *
 * parser.mli
 *
 * This file contains the signature for the parser
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

(* require "lex-s.sml" ;  *)

val getToken : Lex.token -> Lex.token list ->
               (Lex.token list * Lex.token) list ;;

val getTokenRc : Lex.token -> ((Lex.token * string * int * int * int * int) list) ->
                 ((Lex.token * string * int * int * int * int) list * Lex.token * string * int * int * int * int) list ;;

val ( & ) :('a list -> ('a list * 'b) list) ->
           ('a list -> ('a list * 'c) list) -> ('a list -> ('a list * ('b * 'c)) list);;

(*('a list -> ('a list * 'b) list) ->
            ('a list -> ('a list * 'c) list) ->
            (('a list -> ('a list * ('b * 'c))) list) ;;*)

val ( ||| ) : ('a list -> ('a list * 'b) list) ->
              ('a list -> ('a list * 'b) list) ->
              ('a list -> ('a list * 'b) list) ;;

val ( |||| ) : ('a list -> ('a list * 'b) list) ->
               ('a list -> ('a list * 'b) list) ->
               ('a list -> ('a list * 'b) list) ;;

type precedence = LEFT of Lex.token * int | RIGHT of Lex.token * int ;;

val ( ==> ) : (Lex.token list -> (Lex.token list * 'b) list) -> ('b -> 'c) ->
              (Lex.token list -> (Lex.token list * 'c) list) ;;

val precParse: (precedence list) ->
               (Lex.token list -> (Lex.token list * 'b) list) ->
               ('b -> Lex.token -> 'b -> 'b) ->
               Lex.token list -> (Lex.token list * 'b) list ;;





