(******************************************************************************
 *
 * REWRITELIB
 *
 * rewtest.ml 
 *
 * Test functions for rewriting
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

let is_title s = match s with
  | "FUNCTION" -> true
  | "PROPERTIES" -> true
  | "TYPEDEF" -> true
  | "CASES" -> true
  | "TYPEDEF" -> true
  | "CLEAR" -> true
  | x -> (Getfile.is_prefix "ASSOC " x) || (Getfile.is_prefix "COMM " x)
          || (Getfile.is_prefix "AC " x)
          || (Getfile.is_prefix "LESSP" x) ;;

let rec getBlock s l = match l with
  | [] -> (s,[])
  | (""::b) -> getBlock s b
  | (a::b) ->
    if is_title a then (s,(a::b)) else getBlock (s@[a]) b ;;

let rec output_block l = match l with
  | [] -> ()
  | (a::b) -> (print_string a;print_string "\n";
               output_block b) ;;

let processFunction l env = match l with
  | (name::(typ::(pre::r))) ->
    Renv.addFunction env ((Exp.parseExp name),(Type.parse typ),(Exp.parseExp pre),
                (List.map Exp.parseRule r)) []
  | b -> ((output_block b); print_string "\n"; env) ;;

let processProperties r env =
    List.fold_left (fun env -> (fun prop -> Renv.addProperty env (Exp.parseRule prop))) env r ;;

let rec extractLines x n = match (x,n) with
  | (_,0) -> ([],x)
  | ((a::b),n) ->
    let (f,r) = extractLines b (n - 1) in
        (a::f,r)
  ;;

let atoi n = let ([Lex.NUMBER x]) = (Lex.tokenize n) in x ;;

let extractCase (a::n::r) =
    let (l,rest) = extractLines r (atoi n) in
        (Exp.parseExp a,List.map Exp.parseExp l,rest)
    ;;

let rec output_exp_list l = match l with
  | [] -> ()
  | (a::b) ->
        (print_string "    ";print_string (Exp.prExp a);
         print_string "\n";
         output_exp_list b) ;;

let testCase env e r func =
    if func env e = r then
        print_string "."
    else
        (print_string "Error with: ";print_string (Exp.prExp e);
         print_string "\n";
         print_string "Expected:\n";
         output_exp_list r;
         print_string "Got:\n";
         output_exp_list (func env e)) ;;

let rec processCases l env func = match l with
  | [] -> env
  | [x] -> env
  | rr ->
    let (e,r,rest) = extractCase rr in
        (testCase env (Renv.flatten env e) (List.map (Renv.flatten env) r) func;
         processCases rest env func)
    ;;

let rec mergeLines l = match l with
  | [] -> ""
  | (a::b) -> a ^ " " ^ (mergeLines b) ;;

let processTypedef r env =
    Renv.addTypeDefinition env (Type.parseWholeDef (mergeLines r)) ;;

let implode x = List.fold_left (fun x -> (fun y -> x ^ y)) "" x

let rec explode s =
    if String.length s > 0 then
        (String.sub s 0 1)::(explode (String.sub s 1 ((String.length s)-1)))
    else
        []

let rec split_symbol x l = match l with
  | [] -> (Intern.intern (implode x),Intern.intern "")
  | (" "::r) -> (Intern.intern (implode x),Intern.intern (implode r))
  | (a::b) -> split_symbol (x@[a]) b ;;

let processBlock l env func = match l with
  | ("FUNCTION"::r) -> processFunction r env
  | ("PROPERTIES"::r) -> processProperties r env
  | ("CASES"::r) -> processCases r env func
  | ("TYPEDEF"::r) -> processTypedef r env
  | ("CLEAR"::r) -> Renv.emptyEnv
  | (f::r) ->
        if (Getfile.is_prefix "AC" f) then
            Renv.addAttrib env Intern.intern_ac [S(Intern.intern (Getfile.after_space f))]
        else if (Getfile.is_prefix "ASSOC" f) then
            Renv.addAttrib env Intern.intern_a [S(Intern.intern (Getfile.after_space f))]
        else if (Getfile.is_prefix "COMM" f) then
            Renv.addAttrib env Intern.intern_c [S(Intern.intern (Getfile.after_space f))]
        else if (Getfile.is_prefix "LESSP" f) then
            Renv.addPrecedence env (split_symbol []
                (explode (Getfile.after_space f)))
        else ((output_block (f::r));
              (print_string "\n"; env))
  | b ->
        ((output_block b);
         (print_string "\n"; env)) ;;

let rec processBlocks env l func = match l with
  | [] -> (print_string "\n";env)
  | (a::b) ->
    if (is_title a) then
        let (bb,rest) = getBlock [a] b in
            processBlocks (processBlock bb env func) rest func
    else
        processBlocks env b func ;;

let test func file =
    processBlocks Renv.emptyEnv (Getfile.getfile file) func ;;





