(******************************************************************************
 *
 * REWRITELIB
 *
 * rcontext.mli
 *
 * Routines for assisting in the treatment of bound variable contexts
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

(* require "exp.sml" ;  *)

val getFreeVars: Exp.exp -> int list ;;
val getFreeVarsWithIndices: Exp.exp -> (int * (int list)) list ;;
val getAllVars: Exp.exp -> int list ;;
(*
 * Naming away
 *)
val name_away_from_list: (int list) -> int -> int ;;
val name_away: Exp.exp -> int -> int ;;
(*
 * Variable marking
 *)
val getMarkedVars: Exp.exp -> int list ;;
val markVars: Exp.exp -> Exp.exp ;; (* Mark all the free variables in "Exp" *)
val markVarsList: Exp.exp -> int list -> Exp.exp ;; (* Mark all the free variables in "Exp" *)
val unmarkVars: Exp.exp -> Exp.exp ;;
val unmarkVarsList: Exp.exp -> int list -> Exp.exp ;;
val unmarkUnnormalVars: Exp.exp -> Exp.exp ;;
val unmarkVar: int -> Exp.exp -> Exp.exp ;;
(*val unmarkAndName: int -> ((int * int) list) -> Exp.exp -> Exp.exp ;;*)
(*
 * Indexed variable routines
 *)
type indexedVariable ;;
val getIndexedVariableName: indexedVariable -> int ;;
val isFreeIndexedVariable: indexedVariable -> bool ;;
val bindingIndex: indexedVariable -> (int list) ;;
val allIndexedVariables: Exp.exp -> (indexedVariable list) ;;
val allIndexedAndMarkedVariables: Exp.exp -> (indexedVariable list) ;;
val toSubterm: (int list) -> (indexedVariable list) -> (indexedVariable list) ;;
val whichIndexedVariable: Exp.exp -> (int list) -> indexedVariable ;;
(*
 * Bound variable context
 *)
exception NotBound ;;
type context ;;
val emptyContext: context ;;
val depth: string -> context -> int ;;
val addContext: context -> string list -> context ;;
(*
 * Control abstractions
 *)
(*        val mapFreeVar: (int * context -> Exp.exp) -> Exp.exp -> Exp.exp
val foldFreeVar: (('a * int * context) -> 'a) -> 'a -> Exp.exp -> 'a*)

