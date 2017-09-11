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
 * All rights reserved--This is an incomplete work.  An appropriate license
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





