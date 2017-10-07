(******************************************************************************
 *                       
 * REWRITELIB
 *        
 * env.mli  
 *          
 * This module defines the data structure for maintaining all information
 * necessary for describing the environment of the theorem prover.  This
 * includes functions including their type, pre-condition and definition rules,
 * property rules, type definitions, rules resulting from inductive hypothesis
 * introduction and information on which functions are associative, and
 * communitive.  In addition to keeping all of this information, the
 * environment maintains a discrimination net of definition and property
 * rules.
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
(* require "type.sml" ;  *)
(* require "disc.sml" ;  *)
(* require "subst.sml" ;  *)
(* require "pp.sml" ;  *)

type env
type parm = S of int
          | E of Exp.exp
          | IL of (int list)
          | SL of (int list)
          | I of int
          | WILD
          ;;

(*
 * Constructors
 *)
val emptyEnv: env ;;
val addFunction: env -> (Exp.exp * Rtype.etype * Exp.exp * Exp.exp list) -> (Exp.exp list) -> env ;;
val addProperty: env -> Exp.exp -> env ;;
val addViolationRule: env -> Exp.exp -> env ;;
val newDefinition: env -> (int * Exp.exp list) -> env ;;
val addTypeDefinition: env -> (Rtype.etype * Rtype.typeDef) -> env ;;
val addAttrib: env -> int -> (parm list) -> env ;;
val addImported: env -> string -> env ;;
val addVarType: env -> (int * Rtype.etype) -> env ;;
(*val stripVarTypes: env -> (int list) -> env ;;*)
val setNextUsableVar: env -> int -> env ;;
exception CircularPrecedence ;;
val addPrecedence: env -> (int * int) -> env ;;
val addEqualPrecedence: env -> (int * int) -> env ;;
val addNameAways: env -> (int list) -> env ;;
val addSingularRule: env -> Exp.exp -> env ;;
val addGroup: env -> int -> (int list) -> env ;;
val addFilter: env -> int -> (int list) -> env ;;
val addExpanders: env -> int -> (Exp.exp list list) -> env ;;
val addMinorPrecedence: env -> int -> int -> env ;;
val addFailedList: env -> Exp.exp -> env ;;
val addContextRules: env -> (Exp.exp list) -> env ;;
val clearContextRules: env -> env ;;
val addOverload: env -> int -> (int list) -> env ;;
val addConversion: env -> int -> env ;;
(*
 * Environment retrieval operations
 *)
exception UndefinedSymbol of int ;;
val getRules: env -> Disc.disc ;;
val getRulesDoubled: env -> Disc.disc ;;
(*val getAllConstructors: env -> (int list) ;;*)
val getFunctionType: env -> int -> Rtype.etype ;;
val getArity: env -> int -> int ;;
val getFunctionPrecondition: env -> int -> (Exp.exp * Exp.exp) ;;
val getFunctionDefinition: env -> int -> Exp.exp list ;;
val getTypeDefinition: env -> int -> (Rtype.etype * Rtype.typeDef) ;;
val isFiniteType: env -> int -> bool ;;
val getVarType: env -> int -> Rtype.etype ;;
val getAttrib: env -> int -> (parm list) -> (parm list) ;;
val selectAttrib: env -> int -> int -> ((parm list) list) ;;
val allAttrib: env -> int -> (parm list) -> ((parm list) list) ;;
val isFiniteConstructor: env -> int -> bool ;;
val isAC: env -> int -> bool ;;
val isACorC: env -> int -> bool ;;
val isAorC: env -> int -> bool ;;
val isA: env -> int -> bool ;;
val isC: env -> int -> bool ;;
val isEQ: env -> int -> bool ;;
val isOrder: env -> int -> bool ;;
(*val getAC: env -> (string list)*)
val isImported: env -> string -> bool ;;
val getID: env -> int -> Exp.exp ;;
val getNextUsableVar: env -> int ;;
val getType: env -> int -> Rtype.etype ;;
val hasSmallerPrecedence: env -> int -> int -> bool ;;
val hasSmallerMinorPrecedence: env -> int -> int -> bool ;;
val getExpanders: env -> int -> (Exp.exp list list) ;;
val hasEqualPrecedence: env -> int -> int -> bool ;;
val getNameAways: env -> (int list) ;;
val getAllRules: env -> Exp.exp list ;;
val getAllSymbols: env -> int list ;;
val getSmallerSymbols: env -> int -> (int list) ;;
val getEqualSymbols: env -> int -> (int list) ;;
val getGreaterSymbols: env -> int -> (int list) ;;
val getPrecedenceInfo: env -> (string list) ;;
val getFunctions: env -> (string list) ;;
val getTypes: env -> (string list) ;;
val getAttributes: env -> (string list) ;;
val getFunctionDetail: env -> int -> (string list) ;;
val isSingularRule: env -> Exp.exp -> bool ;;
val smallestSymbolOfType: env -> int -> bool ;;
val getFailedList: env -> (Exp.exp list) ;;
val getViolationDisc: env -> Disc.disc ;;
val getContextDisc: env -> Disc.smallDisc ;;
val getContextList: env -> (int list) ;;
val getOverloadChoices: env -> int -> (int list) ;;
val getConversions: env -> (int list) ;;
(*
 * Misc.
 *)
val name_away_bound_vars: Exp.exp -> (int list) -> (Rsubst.unifier * Exp.exp) ;;
val flatten: env -> Exp.exp -> Exp.exp ;;
val flatten_top: env -> Exp.exp -> Exp.exp ;;
val print_env: env -> string ;;

val filterList: env -> int -> ('a list) -> ('a list) ;;

val getForbids: env -> Exp.exp list ;;
val setForbids: env -> Exp.exp list -> env ;;

(*
 * Pretty printer routines
 *)
val addDirective : env -> PrettyPrint.directive -> env ;;
val addParseDirectives : env -> string -> env ;;
val ppExp : env -> Exp.exp -> int ->
            (string list * ((int list * int * int * int * int) list)) ;;
val ppOneLine : env -> Exp.exp -> string ;;
val ppParse : env -> string -> Exp.exp ;;
val ppParsePos : env -> string -> string -> (Exp.exp * (((int list) * string * int * int * int * int) list)) ;;




