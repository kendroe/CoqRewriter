(******************************************************************************
 *
 * REWRITELIB
 *
 * mylist.mli
 *
 * This file contains signatures for some useful list processing functions
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

