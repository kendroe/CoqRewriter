(******************************************************************************
 *                                   
 * REWRITELIB                        
 *                                   
 * type.mli                        
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

type etype ;;
type tyvar ;;
type typeDef ;;

(*
 * Parsing/unparsing
 *)
val unparse: etype -> string ;;
val parse: string -> etype ;;
val parseDef: string -> typeDef ;;
val parseWholeDef: string -> (etype * typeDef) ;;
val unparseDef: typeDef -> string ;;
val emptyDef : typeDef ;;
(*
 * etype constructor and destructor functions
 *)
exception TypeError of (etype * etype * (int list)) ;;
val newVar : unit -> tyvar ;;
val mkVar: tyvar -> etype ;;
val untypeVar: tyvar -> etype -> tyvar ;;
val mkSlot : tyvar -> etype ;;
val untypeSlot : etype -> tyvar ;;
val mkProduct: int -> etype list -> etype ;;
val untypeProduct: int -> int -> etype -> etype list ;;
val nameProduct: etype -> int ;;
val paramProduct: etype -> (etype list) ;;
val mkTfun: etype -> etype -> etype ;;
val untypeTfun: etype -> etype * etype ;;
val notype: etype ;;
val notetype: etype -> bool ;;
val allNames: etype -> int list ;;
val allDefinitionNames: typeDef -> int list ;;
(*
 * Routines to assist type inferencing in expressions
 *)
exception UndefinedConstructor ;;
val instantiateetype: int -> etype -> (int * etype);;
val applyFunctionetype: etype -> etype -> etype ;;
val getConstructoretype: (etype * typeDef) -> int -> etype ;;
val getReturnetype: etype -> etype ;;
val getArgumentetype: etype -> int -> etype ;;
val getArgumentCount: etype -> int ;;
val getetypeName: etype -> int ;;
val getConstructorList: (etype * typeDef) -> (int list) ;;
val isFiniteetype : (etype * typeDef) -> bool;;
(*
 * etype variable substitution
 *)
type subst ;;
exception NoMapping ;;

val id: subst ;;
val addSubst: subst -> tyvar -> etype -> subst ;;
val get: subst -> tyvar -> etype ;;
val printSubst: subst -> unit ;;

val on: etype -> subst -> etype ;;
val rec_on: etype -> subst -> etype ;;

val tmatch: etype -> etype -> subst -> subst ;;
val unify: subst -> etype -> etype -> subst ;;

val pretype: etype -> string ;;






