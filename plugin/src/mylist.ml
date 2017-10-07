(******************************************************************************
 *
 * REWRITELIB
 *
 * list.ml
 *
 * This file contains the implementation of the list functions
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

(* require "list-s.sml" ;  *)

(*infix 2 *| ;;*)
(*infix 2 <| ;;*)
(*infix 2 |> ;;*)
(*infix 2 <> ;;*)
(*infix 3 >< ;;*)

let rec member a l = match l with
  | [] -> false
  | (b::c) -> if a = b then true else member a c ;;

let rec select f l = match l with
  | [] -> []
  | (a::b) -> if (f a) then a::(select f b) else (select f b) ;;

let rec remove_dups l = match l with
  | [] -> []
  | (a::b) -> if member a b then remove_dups b
              else a::(remove_dups b) ;;

let rec difference l x = match l with
  | [] -> []
  | (a::b) -> if member a x then difference b x
              else a::(difference b x) ;;

let rec delete a l = match l with
  | [] -> []
  | (b::c) -> if a = b then delete a c else
              b::(delete a c);;

let rec delete_one a l = match l with
  | [] -> []
  | (b::c) -> if a = b then c else
              b::(delete_one a c);;

let rec intersect l x = match l with
  | [] -> []
  | (a::b) -> if member a x then a::(intersect b x)
              else intersect b x ;;

let rec append l x = match l with
  | [] -> x
  | (a::b) -> a::(append b x) ;;

let rec is_subset l x = match l with
  | [] -> true
  | (a::b) -> if (member a x) then is_subset b (delete a x)
              else false ;;

let ( *| ) f1 f2 x = List.fold_left append [] (List.map f2 (f1 x)) ;;
let ( <| ) l f = List.fold_left append [] (List.map f l) ;;
let ( <> ) a b = List.map b a ;;
let rec ( |> ) l f = 
     match l with
     | [] -> []
     | (a::b) -> if (f a) then a::(b |> f) else (b |> f) ;;

let rec cross x l1 l2 = match (x,l1,l2) with
  | (_,_,[]) -> []
  | (x,(a::b),(c::d)) -> (a,c)::(cross x b (c::d))
  | (x,[],(c::d)) -> cross x x d ;;

let ( >< ) a b = cross a a b ;;

let x f y = [f y] ;;

let rec cross_list l = match l with
    | [] -> [[]]
    | (a::b) -> a >< (cross_list b) <> (fun (a,b) -> a::b) ;;

let rec replace_nth l n x = match (l,n) with
    | ((a::b),0) -> (x::b)
    | ((a::b),n) -> a::(replace_nth b (n-1) x)
    | ([],n) -> [];;

let rec delete_nth l n = match (l,n) with
    | ((a::b),0) -> b
    | ((a::b),n) -> a::(delete_nth b (n-1))
    | ([],n) -> [];;

let rec pair_lists l m = match (l,m) with
  | ([],[]) -> []
  | ((a::b),(c::d)) -> (a,c)::(pair_lists b d)
  | (_,_) -> [];;

let rec parent l = match l with
  | [a] -> []
  | [] -> []
  | (a::b) -> a::(parent b);;

let rec is_prefix_list l p = match (l,p) with
  | ([],p) -> true
  | ((a::b),(c::d)) -> if a=c then is_prefix_list b d else false
  | (x,y) -> false;;

let rec tail l n = match (l,n) with
  | ((a::b),n) -> if (n>0) then tail b (n-1) else (a::b)
  | ([],n) -> [];;

