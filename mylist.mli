(******************************************************************************)
 *
 * REWRITELIB
 *
 * list.mli
 *
 * This file contains signatures for some useful list processing functions
 *
 * (C) 2017, Kenneth Roe
 *
 * All rights reserved--This is an incomplete work.  An appropriate license
 * will be provided when the work is complete.
 *
 * For a commercial license, contact Roe Mobile Development, LLC at
 * info@roemobiledevelopment.com
 *
 ******************************************************************************)

val member: 'a -> 'a list -> bool
val select: ('a -> bool) -> 'a list -> 'a list
val remove_dups: 'a list -> 'a list
val difference: 'a list -> 'a list -> 'a list
val delete: 'a -> 'a list -> 'a list
val delete_one: 'a -> 'a list -> 'a list
val intersect: 'a list -> 'a list -> 'a list
val append: 'a list -> 'a list -> 'a list
val is_subset: 'a list -> 'a list -> bool
val replace_nth: ('a list) -> int -> 'a -> ('a list)
val delete_nth: 'a list -> int -> ('a list)
val ( *| ) : ('a -> 'b list) -> ('b -> 'c list) -> ('a -> 'c list)
val x: ('a -> 'b) -> ('a -> 'b list)
val ( <| ) : ('a list) -> ('a -> 'b list) -> 'b list
val ( <> ) : ('a list) -> ('a -> 'b) -> 'b list
val ( |> ) : ('a list) -> ('a -> bool) -> 'a list
val ( >< ) : ('a list) -> ('b list) -> (('a * 'b) list)
val cross_list : ('a list list) -> ('a list list)
val pair_lists : ('a list) -> ('b list) -> (('a * 'b) list)
val parent : ('a list) -> ('a list)
val is_prefix_list : ('a list) -> ('a list) -> bool
val tail : ('a list) -> int -> ('a list)

