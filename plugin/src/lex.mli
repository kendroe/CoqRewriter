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

