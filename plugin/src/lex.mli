(******************************************************************************
 *                                   
 * REWRITELIB                        
 *                                   
 * lex.mli                        
 *                                   
 * Signature for lexical analysis module
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

type token = ID of string
              | SYMBOL of string
              | QUOTE of string
              | C_QUOTE of char
              | NUMBER of int
              | RAT of int * int
              | SPECIAL of string
              | TVAR of string * string * int ;;

exception Failure of token list ;;

val tokenize_rc: string -> string -> (token * string * int * int * int * int) list ;;
val tokenize: string -> token list ;;
val print_tokens: (token list) -> string ;;
val print_tokens2: ((token * string * int * int * int * int) list) -> string ;;

