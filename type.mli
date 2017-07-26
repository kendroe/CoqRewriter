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
 * All rights reserved--This is an incomplete work.  An appropriate license
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






