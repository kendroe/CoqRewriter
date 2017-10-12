(******************************************************************************
 *                       
 * REWRITELIB
 *          
 * builtin.mli
 *              
 * This file contains heuristic rewriting.
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
   
(* require "env.sml" ;  *)
(* require "exp.sml" ;  *)

val builtin: (Renv.env -> Exp.exp -> (Exp.exp list)) -> Renv.env -> Exp.exp -> (Exp.exp list) ;;




