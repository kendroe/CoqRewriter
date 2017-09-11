(******************************************************************************
 *
 * REWRITELIB
 *
 * disc.mli
 *
 * Signature for discrimination net package
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

(* require "exp.sml" ;  *)

type disc ;;
type 'a expDisc ;;
type ('a,'b) listDisc ;;
type smallDisc ;;

val newSmall: smallDisc ;;
val addSmall: smallDisc -> (int list) -> smallDisc ;;
val findSmall: smallDisc -> Exp.exp -> (Exp.exp list) ;;

val newDisc: disc ;;
val add: (int -> bool) -> disc -> Exp.exp -> disc ;;
val makeAC: int -> disc -> disc ;;
val find: (int -> bool) -> disc -> Exp.exp -> Exp.exp list ;;
val ufind: (int -> bool) -> disc -> Exp.exp -> Exp.exp list ;;
val delete_exp: disc -> int -> disc ;;
val makedcstring: disc -> string ;;
val allExps: disc -> (Exp.exp list) ;;

val addItem: ('a,'b) listDisc -> ('a list) -> 'b -> (('a,'b) listDisc) ;;
val findItem: ('a,'b) listDisc -> ('a list) -> ('b list) ;;
val newItem: ('a,'b) listDisc ;;

val addExpItem: 'a expDisc -> Exp.exp -> 'a -> ('a expDisc) ;;
val findExpItem: 'a expDisc -> Exp.exp -> ('a list) ;;
val newExpItem: 'a expDisc ;;





