(******************************************************************************
 *
 * REWRITELIB
 *
 * crewrite.mli
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
(* require "env.sml" ;  *)

(*val orient_rule: env -> Exp -> Exp*)
val create_rules: (Exp.exp -> Exp.exp list) -> Renv.env -> Exp.exp -> int -> Renv.env
val relevant_rule: Renv.env -> Exp.exp -> Exp.exp -> bool
val filter_rule_list: Renv.env -> Exp.exp -> (int list) -> (int list)
val junction_filter_rule_list: Renv.env -> Exp.exp -> (int list) -> (int list)




