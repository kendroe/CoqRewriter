(******************************************************************************
 *
 * REWRITELIB
 *
 * plugin.ml4
 *
 * This file contains the interface between Coq and the rewriting library
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

DECLARE PLUGIN "theplug"

open Stdarg
open Ltac_plugin

module Stuff = struct
(*
 * Plugin to print an s-expression representing the (possibly expanded) AST for a definition.
 * Based on TemplateCoq: https://github.com/gmalecha/template-coq/blob/master/src/reify.ml4
 *
 * Consider this plugin a learning tool to help understand how to traverse the AST, which is why it is extensively commented.
 * It's also why even simple functions are highly separated out instead of nested, which is not typical OCaml style.
 * Feel free to fork it and mess around with the functions to see what happens.
 *)

open Declarations
open Format
open Univ
open Term
open Names

open Pp
open CErrors
open Util
open Names
open Nameops
open Term
open Termops
open Declarations
open Environ
open Impargs
open Libobject
open Libnames
open Globnames
open Recordops
open Misctypes
open Printer
open Printmod
open Context.Rel.Declaration


open Intern
open Exp

let debug_ids = []

let debug_print c s =
    if (List.mem "ALL" debug_ids) || (List.mem c debug_ids) then
        print_string (c ^ ": " ^ s ^ "\n")
    else
        ()

module StringConstr =
       struct
         type t = string
         let compare s t = String.compare s t
       end

module StringMap = Map.Make(StringConstr)

exception NoEntry;;

let constr_cache : Constr.t StringMap.t ref = ref (StringMap.empty) ;;

let get_constr s =
    debug_print "get_constr" ("Finding " ^ s);
    try (StringMap.find s (!constr_cache)) with
    Not_found -> (debug_print "get_constr" "Failed";raise NoEntry) ;;

let add_constr s c =
    debug_print "add_constr" ("Adding " ^ s);
    constr_cache := StringMap.add s c (!constr_cache) ;;

let def_cache : Constr.t StringMap.t ref = ref (StringMap.empty) ;;

let get_def s =
    debug_print "get_def" ("Finding " ^ s);
    try (StringMap.find s (!def_cache)) with
    Not_found -> (debug_print "get_def" "Failed2";raise NoEntry) ;;

let add_def s c =
    debug_print "add_def" ("Adding " ^ s);
    def_cache := StringMap.add s c (!def_cache) ;;


(* --- Options --- *)

(*
 * Toggles between DeBruijn indexing and names
 *)
let opt_debruijn = ref (false)
let _ = Goptions.declare_bool_option {
  (*Goptions.optsync = true;*)
  Goptions.optdepr = false;
  Goptions.optname = "DeBruijn indexing in PrintAST";
  Goptions.optkey = ["PrintAST"; "Indexing"];
  Goptions.optread = (fun () -> !opt_debruijn);
  Goptions.optwrite = (fun b -> opt_debruijn := b);
}

let is_debruijn () = !opt_debruijn

(*
 * Toggles showing explicit universe instances for universe polymorphic constants
 *)
let opt_show_universes = ref (false)
let _ = Goptions.declare_bool_option {
  (*Goptions.optsync = true;*)
  Goptions.optdepr = false;
  Goptions.optname = "Show universe instances in PrintAST";
  Goptions.optkey = ["PrintAST"; "Show"; "Universes"];
  Goptions.optread = (fun () -> !opt_show_universes);
  Goptions.optwrite = (fun b -> opt_show_universes := b);
}

let show_universes () = !opt_show_universes

(* --- Helper functions : Printing --- *)

(*
 * Prints a string using the Coq pretty printer
 *)
let print (s : string) = print_string s
  (*Pp.pp (Pp.str s)*)

(*
 * Using a supplied pretty printing function, prints directly to a string
 *)
let print_to_string (pp : formatter -> 'a -> unit) (trm : 'a) =
  Format.asprintf "%a" pp trm

(*
 * Pretty prints a constructor
 *)
let pp_constr (fmt : formatter) (c : constr)  =
  Pp.pp_with fmt (Printer.pr_constr c)

(*
 * Pretty prints a universe level
 *)
let pp_univ_level (fmt : formatter) (l : Level.t) =
  Pp.pp_with fmt (Level.pr l)

(*
 * Wraps a string in parens
 *)
let wrap (s : string) =
  String.concat "" ["(" ; s ; ")"]

(*
 * Builds an s-expression string from a string representing the node
 * of the AST and a list of strings representing the leaves
 *)
let build (node : string) (leaves : string list) =
  wrap (String.concat " " (node :: leaves))

(* --- Other helper functions --- *)

(*
 * Creates a list so we can map over the range of a to b
 * This is an auxliary function renamed from seq in template-coq
 *)
let rec range (min : int) (max : int) =
  if min < max then
    min :: range (min + 1) max
  else
    []

(* --- Names --- *)

(*
 * A Name identifies a binding
 * Names are either identifiers (id : T) or anonymous bindings (_ : T)
 * Names are assigned to new bindings and can be retrieved by indexes from an environment
 *)

(*
 * Build an Exp for a name
 *)
let build_name (n : name) =
  match n with
    Name id -> build "Name" [string_of_id id]
  | Anonymous -> "(Anonymous)"

let build_exp_name (n : name) =
  match n with
    Name id -> VAR (intern (string_of_id id))
  | Anonymous -> VAR (intern "(Anonymous)")


(* --- Variables --- *)

(*
 * A variable is identified by a name that may not be Anonymous
 *)

(*
 * Build the AST for a variable
 *)
let build_var (v : identifier) =
  build "Var" [string_of_id v]

let build_var_exp (v : identifier) =
  VAR (intern (string_of_id v))

(* --- Metavariables --- *)

(*
 * A metavariable is an unknown ?n
 * A metavariable is represented by an integer
 *
 * This is only useful if you are extending this plugin
 * For now we don't actually do anything with it except print the index
 * It doesn't look like these are DeBruijn indexes -- rather each metavariable has a unique int
 *)

let build_meta (n : metavariable) =
  build "Meta" [string_of_int n]

let build_exp_meta (n : metavariable) =
  VAR (intern ("(Meta_"^ (string_of_int n) ^ ")"))

(* --- Existential variables --- *)

(*
 * Existential variables are basically integers, like metavariables
 * It's unclear to me why these exist separately from metavariables in the AST, since the words are used interchangeably in comments
 * The existential_key technically has a different type, but this is only for the kernel
 *
 * These are also only useful if you are extending the plugin
 * I have no clue why existentials and metavariables are distinct in the AST
 * I also don't know what the array of constructors is for -- if you figure this all out please submit a pull request!
 *)

let build_evar (k : existential_key) (c_asts : string list) =
  build "Evar" ((string_of_int (Evar.repr k)) :: c_asts)

let build_evar_exp (k : existential_key) (c_asts : exp list) =
  (APPL ((intern (string_of_int (Evar.repr k))), c_asts))

(* --- Indexes --- *)

(*
 * A Rel identifies a binding with De Bruijn indexing
 * We can push bindings to the environment using Environ.push_rel
 * We can retrieve names using Environ.lookup_rel
 *)

(*
 * Retrieve a variable from a Rel and build the AST
 *
 * Looking up a Rel produces a (name, body, type) triple
 * The body and type are arbitrary terms
 * For now we don't recursively print them, but we expose them here for clarity
 *
 * The name may be Anonymous, in which case we print the index
 *)
let build_rel_named (env : Environ.env) (i : int) =
  try let r = Environ.lookup_rel i env in
      let name = Context.Rel.Declaration.get_name r in
      (*let (name, body, typ) = Environ.lookup_rel i env in*)
      build_name name with Not_found -> build "NFRel" [string_of_int i]

(*
 * Build an AST for a Rel
 *
 * If De Bruijn mode is on, we show the index
 * Otherwise, we show the Name the index refers to in the environment
 *)
let build_rel (env : Environ.env) (i : int) =
  if is_debruijn () then
    build "Rel" [string_of_int i]
  else
    build_rel_named env i

let build_rel_exp (env : Environ.env) (i : int) =
    let r = Environ.lookup_rel i env in
    let name = Context.Rel.Declaration.get_name r in
    (*let (name, body, typ) = Environ.lookup_rel i env in*)
    let n = (match name with
                 Name id -> string_of_id id
               | Anonymous -> "(Anonymous)") in
    VAR (intern n)

(* --- Universes --- *)

(*
 * Universes represent the Coq universe hierarchy
 * Every universe either represents a universe level (Level.t) or is algebraic
 * Algebraic universes represent the max of their universe levels
 *)

(*
 * Build the AST for a universe level
 *)
let build_universe_level (l : Level.t)  =
  build "Level" [print_to_string pp_univ_level l]

let build_universe_level_exp (l : Level.t)  =
  APPL ((intern "Level"),[VAR (intern (print_to_string pp_univ_level l))])

(*
 * Build the AST for the universe levels of a universe
 *
 * If the universe is a level, then print that level
 * Otherwise take the max of the levels of the algebraic universe
 *)
let build_universe_levels (u : universe) =
  match Universe.level u with
    Some l -> build_universe_level l
  | None -> build "Max" (List.map build_universe_level (LSet.elements (Universe.levels u)))

let build_universe_levels_exp (u : universe) =
  match Universe.level u with
    Some l -> build_universe_level_exp l
  | None -> APPL ((intern "Max"),(List.map build_universe_level_exp (LSet.elements (Universe.levels u))))

(*
 * Build the AST for a universe
 *)
let build_universe (u : universe) =
  build "Universe" [build_universe_levels u]

let build_universe_exp (u : universe) =
  APPL ((intern "Universe"),[build_universe_levels_exp u])

(* --- Universe instances --- *)

(*
 * Universe instances exist to support universe polymorphism, which was added in 8.5
 * You will probably rarely see this in practice
 *
 * Documentation: https://coq.inria.fr/refman/Reference-Manual032.html
 *)

(*
 * Build an AST for a universe instance when printing universe instances is enabled
 *)
let build_universe_instance (i : Instance.t) =
  let ls = Instance.to_array i in
  build "UnivInstance" (List.map build_universe_level (Array.to_list ls))

(* --- Sorts --- *)

(*
 * Gallina has three sorts: Prop, Set, and Type
 * Prop is the impredicative sort where logical propositions live
 * Set is at the bottom of the hierarchy
 * Type is really an infinite universe hierarchy of Types, but you usually don't see this
 *
 * A good read, though a bit dated, is this chapter of CPDT: http://adam.chlipala.net/cpdt/html/Universes.html
 *)

(*
 * Build the AST for a sort
 *)
let build_sort (s : sorts) =
  let s_ast =
    match s with
      Prop _ -> if s = prop_sort then "Prop" else "Set"
    | Type u -> build "Type" [build_universe u]
  in build "Sort" [s_ast]

let build_sort_exp (s : sorts) =
  let s_ast =
    match s with
      Prop _ -> if s = prop_sort then (APPL ((intern "Prop"),[])) else (APPL ((intern "Set"),[]))
    | Type u -> APPL ((intern "Type"),[build_universe_exp u])
  in APPL ((intern "Sort"),[s_ast])

(* --- Casts --- *)

(*
 * A cast (t : T) enforces that the term t has type T
 * Besides t and T, a cast has a third argument, the cast_kind
 * This argument is just used to determine how to check a Cast
 * You won't see it in Gallina code
 *)

(*
 * Represent the kind of a cast as a string
 *)
let build_cast_kind (k : cast_kind) =
  match k with
    VMcast -> "VMcast"
  | DEFAULTcast -> "DEFAULTcast"
  | REVERTcast -> "REVERTcast"
  | NATIVEcast -> "NATIVEcast"

let build_cast_kind_exp (k : cast_kind) =
  match k with
    VMcast -> (APPL (intern "VMcast",[]))
  | DEFAULTcast -> (APPL (intern "DEFAULTcast",[]))
  | REVERTcast -> (APPL (intern "REVERTcast",[]))
  | NATIVEcast -> (APPL (intern "NATIVEcast",[]))

(*
 * Build the AST for a cast
 *)
let build_cast (trm_ast : string) (kind : cast_kind) (typ_ast : string) =
  build "Cast" [trm_ast; build_cast_kind kind; typ_ast]

let build_cast_exp (trm_ast : exp) (kind : cast_kind) (typ_ast : exp) =
  APPL ((intern "Cast"),[trm_ast; build_cast_kind_exp kind; typ_ast])

(* --- Product types and lambdas --- *)

(*
 * Dependent product types (forall) map every (t : T) to some body
 * Lambdas are functions that take a (t : T) to some body (the type of a lambda is a product)
 *
 * In both cases, t is a Name (Anonymous or an identifier) and T is its Type
 * The body is a type that may refer to the name t
 *
 * Since the body can refer to the name, product types and lambdas create new bindings
 * We'll see how to do this later when traversing the AST
 *)

(*
 * Build the AST for a product
 *)
let build_product (n : name) (typ_ast : string) (body_ast : string) =
  build "Prod" [build_name n; typ_ast; body_ast];;

let map_type t = match (decode t) with
  | "Coq.Init.Datatypes.nat" -> intern "Natural"
  | "Coq.Init.Datatypes.bool" -> intern "Bool"
  | _ -> t ;;

let rec convert_exp_to_type e =
  debug_print "convert_exp_to_type" ("Convert_exp_to_type " ^ (prExp e)) ;
  match e with
  | (APPL (f,l)) -> Rtype.mkProduct (map_type f) (List.map convert_exp_to_type l)
  | (QUANT (14,[(v,t)],e,p)) -> Rtype.mkTfun t (convert_exp_to_type e)
  | _ -> Rtype.notype ;;

let build_product_exp (n : name) (typ_ast : exp) (body_ast : exp) =
  (match n with
   | Anonymous -> APPL (intern_implies,[typ_ast; body_ast])
   | Name n -> QUANT (intern_all,[(intern (string_of_id n),convert_exp_to_type typ_ast)],body_ast,(APPL (intern_true,[])))
   );;

(*
 * Build the AST for a lambda
 *)
let build_lambda (n : name) (typ_ast : string) (body_ast : string) =
  build "Lambda" [build_name n; typ_ast; body_ast]

let build_lambda_exp (n : name) (typ_ast : exp) (body_ast : exp) =
  let nn = match n with
           | Name id -> (intern (string_of_id id))
           | Anonymous -> (intern "(Anonymous)") in
      (QUANT (intern_lambda,[(nn, convert_exp_to_type typ_ast)], body_ast, (APPL (intern_true,[]))))

(* --- Let --- *)

(*
 * Let expressions bind a name with a type (t : T) to some expression in a body
 * Let expressions also create new bindings
 *)

(*
 * Build the AST for a let expression
 *)
let build_let_in (n : name) (trm_ast : string) (typ_ast : string) (body_ast : string) =
  build "LetIn" [build_name n; trm_ast; typ_ast; body_ast]

(* --- Application --- *)

(*
 * Function application applies a function f to some arguments
 * In the Coq kernel these arguments are represented as an Array
 *)

(*
 * Build the AST for function application
 *)
let build_app (f_ast : string) (arg_asts : string list) =
  build "App" (f_ast :: arg_asts)

(* --- Constants --- *)

(*
 * A Const is a global constant (axiom, definition, and so on)
 * It is represented by a pair (c, u)
 *
 * From c you can get its canonical name (kernel_name)
 * You can also look it in the environment to get the definition and type that the constant represent
 * A bodyless definition is an axiom
 *
 * The element u is just a universe instance when it's universe polymorphic
 *)

(*
 * Build the AST for a kernel name of a constant
 *)
let build_kername (kn : kernel_name) =
  string_of_kn kn

(*
 * Get the definition for a constant, forcing it to a Constr
 *)
let get_definition (cd : Declarations.constant_body) =
 match cd.const_body with
   Undef _ ->
     None
 | Def cs ->
     Some (Mod_subst.force_constr cs)
 | OpaqueDef o -> (* https://coq.inria.fr/refman/Reference-Manual008.html#Opaque*) None
     (*Some (Opaqueproof.force_proof (Global.opaque_tables ()) o)*)

(*
 * Build the AST for an axiom, which is a constant with no associated body
 *)
let build_axiom_exp (kn : kernel_name) (typ_ast : exp) (u : Instance.t) =
  let kn' = build_kername kn in
    APPL ((intern "Axiom"),[APPL ((intern kn'),[typ_ast])])

let build_axiom (kn : kernel_name) (typ_ast : string) (u : Instance.t) =
  let kn' = build_kername kn in
  if show_universes () then
    build "Axiom" [kn'; typ_ast; build_universe_instance u]
  else
    build "Axiom" [kn'; typ_ast]

(*
 * Build the AST for a definition
 *)
let build_definition (kn : kernel_name) (typ_ast : string) (u : Instance.t) =
   let kn' = build_kername kn in
   if show_universes () then
     build "Definition" [kn'; typ_ast; build_universe_instance u]
   else
     build "Definition" [kn'; typ_ast]

let build_definition_exp (kn : kernel_name) (typ_ast : exp) (u : Instance.t) =
   let kn' = build_kername kn in
     APPL ((intern"Definition"),[APPL ((intern kn'),[typ_ast])])

(* --- Fixpoints --- *)

(*
 * A fixpoint is (possibly unary) list of mutually recursive functions
 * Each function has an index, a binder, a type, and a body
 * The fixpoint has a final index which denotes the actual fixpoint we care about
 * When there is only one recursive function in the fixpoint, this index is implicitly 1
 *
 * A CoFixpoint has basically the same structure, except that it is useful for representing an infinite stream of data
 *
 * Right now we explicitly write out the index instead of retrieving the definition we care about
 *)

(*
 * A fixpoint also creates bindings that we need to push to the environment
 * This function gets all of those bindings
 *)
let bindings_for_fix (names : name array) (typs : constr array) =
  Array.to_list
    (CArray.map2_i
      (fun i name typ -> (Context.Rel.Declaration.LocalAssum (name, (Vars.lift i typ))))
      names typs)

(*
 * Build the AST for a function in a fixpoint
 *)
let build_fix_fun (index : int) (n : name) (typ_ast : string) (body_ast : string) =
  build (build_name n) [string_of_int index; typ_ast; body_ast]

let build_fix_fun_exp (index : int) (n : name) (typ_ast : exp) (body_ast : exp) =
  APPL ((intern (build_name n)),[NUM index; typ_ast; body_ast])

(*
 * Build the AST for a fixpoint
 *)
let build_fix (funs : string list) (index : int) =
  build "Fix" [build "Functions" funs; string_of_int index]

let build_fix_exp (funs : exp list) (index : int) =
  APPL ((intern_fix),[APPL ((intern "Functions"),funs); NUM index])

(*
 * Build the AST for a cofixpoint
 *)
let build_cofix (funs : string list) (index : int) =
  build "CoFix" [build "Functions" funs; string_of_int index]

let build_cofix_exp (funs : exp list) (index : int) =
  APPL ((intern "CoFix"),[APPL ((intern "Functions"),funs); NUM index])

(* --- Inductive types --- *)

(*
 * Inductive types are actually represented as mutually inductive types
 * In the case of a normal inductive type, there will only be one inductive type in the body
 * Every element of the body is a one_inductive_type, which is a non-mutually inductive type
 *
 * I was pretty confused at first by "lookup_mind" and all similar functions
 * "mind" here refers to "mutually inductive"
 * Keep that in mind
 *)

(*
 * Get the body of a mutually inductive type
 *)
let lookup_mutind_body (i : mutual_inductive) (env : Environ.env) =
  (debug_print "lookup_mutind_body" (MutInd.debug_to_string i);
   Environ.lookup_mind i env)

(*
 * Given an inductive type, the AST just for its name without recursing further
 *)
let build_inductive_name (ind_body : one_inductive_body) =
  let name_id = ind_body.mind_typename in
  build_name (Names.Name name_id)

(*
 * Inductive types also create bindings that we need to push to the environment
 * This function gets those bindings
 *)
let bindings_for_inductive (env : Environ.env) (mutind_body : mutual_inductive_body) (ind_bodies : one_inductive_body list) =
  let m = List.map
    (fun ind_body ->
      let univ_context = match mutind_body.mind_universes with
                         | Monomorphic_ind x -> x
                         | _ -> Univ.UContext.empty in
      let univ_instance = UContext.instance univ_context in
      let name_id = ind_body.mind_typename in
      let mutind_spec = (mutind_body, ind_body) in
      let typ = Inductive.type_of_inductive env (mutind_spec, univ_instance) in
      (Names.Name name_id, None, typ))
    ind_bodies in
  List.fold_left (fun a -> fun (n,_,t) -> Context.Rel.add (LocalAssum (n,t)) a) (Context.Rel.empty) m

(*
 * Build an AST for a mutually inductive type
 *)
let build_inductive (ind_or_coind : Decl_kinds.recursivity_kind) (body_asts : string list) (u : Instance.t) =
  let kind_of_ind =
    match ind_or_coind with
      Finite -> "Inductive"
    | CoFinite -> "CoInductive"
    | BiFinite -> "Record"
  in
  if show_universes () then
    build kind_of_ind (List.append body_asts [build_universe_instance u])
  else
    build kind_of_ind body_asts

let build_inductive_exp (ind_or_coind : Decl_kinds.recursivity_kind) (body_asts : exp list) (u : Instance.t) =
  let kind_of_ind =
    match ind_or_coind with
      Finite -> "Inductive"
    | CoFinite -> "CoInductive"
    | BiFinite -> "Record"
  in
    (APPL ((intern kind_of_ind),body_asts))

(*
 * Build an AST for a single inductive body
 *)
let build_inductive_body (constr_asts : string list) =
  build "inductive_body" constr_asts

let build_inductive_body_exp (constr_asts : exp list) =
  APPL ((intern "inductive_body"),constr_asts)

(* --- Inductive constructors --- *)

(*
 * Each inductive body contains a list of constructors
 * To actually reference a constructor, you use (Constr index) which gets the
 * constructor with that index from the inductive type
 *
 * For now for printing uses of constructors we just print index instead of getting its name
 *)

(*
 * Get the named constructors from an inductive definition
 *)
let named_constructors (ind_body : one_inductive_body) =
  let constr_names = Array.to_list ind_body.mind_consnames in
  let indexes = List.map string_of_int (range 1 ((List.length constr_names) + 1)) in
  let constrs = Array.to_list ind_body.mind_user_lc in
  List.combine indexes (List.combine constr_names constrs)

(*
 * Build the AST for a use of a constructor
 *)
let build_constructor (t_ast : string) (index : int) (u : Instance.t) =
  let index' = string_of_int index in
  if show_universes () then
    build "Construct" [t_ast; index'; build_universe_instance u]
  else
    build "Construct" [t_ast; index']

let build_constructor_exp (t_ast : exp) (index : int) (u : Instance.t) =
    APPL ((intern "Construct"),[t_ast; NUM index])

(* --- Pattern matching --- *)

(*
 * A Case expression is used for pattern matching
 * Every Case expression has a type, a type it matches against, and a list of branches
 *
 * Each Case expression also has a case_info, which is basically metadata
 * It contains the number of arguments, the number of pattern variables of each constructor, and printing information
 * Right now we only use the number of arguments from this
 *)

(*
 * Build an AST for a Case expression
 *)
let build_case (info : case_info) (case_typ_ast : string) (match_ast : string) (branch_asts : string list) =
  let num_args = string_of_int info.ci_npar in
  let match_typ = build "CaseMatch" [match_ast] in
  let branches = build "CaseBranches" branch_asts in
  build "Case" [num_args; case_typ_ast; match_typ; branches]

let getConstructors (t : Rtype.etype) (branch_asts : exp list) =
    let name = decode (Rtype.nameProduct t) in
    let rec bc e n l = match e with
                     | (QUANT (intern_lambda,([(v,t)]),ex,te)) ->
                       bc ex n (l@[v])
                     | e -> ((APPL (intern (name ^ (string_of_int n)),List.map (fun x -> VAR x) l)),e) in
    let rec bcons e l = match e with
                        | (QUANT (intern_lambda,([(v,t)]),ex,te)) ->
                          bcons ex (l@[v])
                        | e -> ((APPL (intern_cons,List.map (fun x -> VAR x) l)),e) in
    let rec ge e = match e with
                   | (QUANT (intern_lambda,([(v,t)]),ex,te)) ->
                     ge ex
                   | e -> e in
    let rec bf bl n = match bl with
                    | [] -> []
                    | (f::r) -> (bc f n [])::(bf r (n+1)) in
        let _ = debug_print "build_case" ("CASE TYPE " ^ name) in
        if name="Natural" then
            let (QUANT (intern_lambda,([(v,t)]),ex,te)) = List.nth branch_asts 1 in
                [((NUM 0),(List.nth branch_asts 0));((APPL (intern "S",[VAR (intern "n")])),ex)]
        else if name="Bool" then
            [((APPL (intern_true,[])),(List.nth branch_asts 0));((APPL (intern_false,[])),(List.nth branch_asts 1))]
        else if name="C_Coq.Init.Datatypes.list" then
             let (cv,cc) = bcons (List.nth branch_asts 1) [] in
            [((APPL (intern_nil,[])),(List.nth branch_asts 0));(cv,cc)]
        else bf branch_asts 1

let build_case_exp (info : case_info) (case_typ_ast : exp) (match_ast : exp) (branch_asts : exp list) =
  try
      let num_args = info.ci_npar in
      let match_typ = APPL ((intern "CaseMatch"),[match_ast]) in
      let (QUANT (l,[(v,t)],_,_)) = case_typ_ast in
      let _ = debug_print "build_case_exp" ((Rtype.pretype t)) in
      let branches = APPL ((intern "CaseBranches"),branch_asts) in
      CASE (match_ast,convert_exp_to_type case_typ_ast,(getConstructors t branch_asts))
  with (Rtype.TypeError(_,_,_)) -> NOEXP
  (*APPL ((intern "Case"),[NUM num_args; t; match_typ; branches])*)

(* --- Projections --- *)

(*
 * A Proj is a constant that must be transparent
 * From the projection element in the tuple, you can get a body (lookup_projection in the environment)
 * You can also retrieve the underlying constant, which is what we do here for now
 *
 * If a Proj is just a constant, I have no clue why it has an extra 'constr in its type
 * Submit a pull request if you figure it out
 * I also haven't tested this yet so I don't know what it prints
 *)

let build_proj (p_const_ast : string) (c_ast : string) =
  build "Proj" [p_const_ast; c_ast]

let build_proj_exp (p_const_ast : exp) (c_ast : exp) =
  APPL ((intern "Proj"),[p_const_ast; c_ast])

let case_infos : (case_info list) ref = ref [] ;;

(* --- Full AST --- *)

let rec build_ast (env : Environ.env) (depth : int) (trm : types) =
  match kind_of_term trm with
    Rel i ->
      (debug_print "build_ast" "Rel";
      build_rel env i)
  | Var v ->
      (debug_print "build_ast" "Var";
       build_var v)
  | Meta mv ->
      (debug_print "build_ast" "Meta";
       build_meta mv)
  | Evar (k, cs) ->
      let _ = debug_print "build_ast" "Evar" in
      let cs' = List.map (build_ast env depth) (Array.to_list cs) in
      build_evar k cs'
  | Sort s ->
      (debug_print "build_ast" "Sort";build_sort s)
  | Cast (c, k, t) ->
      let _ = debug_print "build_ast" "Cast" in
      let c' = build_ast env depth c in
      let t' = build_ast env depth t in
      build_cast c' k t'
  | Prod (n, t, b) ->
      let _ = debug_print "build_ast" "Prod" in
      let t' = build_ast env depth t in
      let b' = build_ast (Environ.push_rel (LocalAssum (n, t)) env) depth b in
      build_product n t' b'
  | Lambda (n, t, b) ->
      let _ = debug_print "build_ast" "Lambda" in
      let t' = build_ast env depth t in
      let b' = build_ast (Environ.push_rel (LocalAssum (n, t)) env) depth b in
      build_lambda n t' b'
  | LetIn (n, trm, typ, b) ->
      let _ = debug_print "build_ast" "LetIn" in
      let trm' = build_ast env depth trm in
      let typ' = build_ast env depth typ in
      let b' = build_ast (Environ.push_rel (LocalDef (n,trm,typ)) env) depth b in
      build_let_in n trm' typ' b'
  | App (f, xs) ->
      let _ = debug_print "build_ast" "App" in
      let f' = build_ast env depth f in
      let xs' = List.map (build_ast env depth) (Array.to_list xs) in
      build_app f' xs'
  | Const (c, u) ->
      let _ = debug_print "build_ast" "Const" in
      build_const env depth (c, u)
  | Construct ((i, c_index), u) ->
      let _ = debug_print "build_ast" "Construct" in
      let i' = build_ast env depth (Term.mkInd i) in
      build_constructor i' c_index u
  | Ind ((i, i_index), u) ->
      (debug_print "build_ast" "Ind";
       build_minductive env depth ((i, i_index), u))
  | Case (ci, ct, m, bs) ->
      let _ = debug_print "build_ast" "Case" in
      let _ = (case_infos := (ci::(!case_infos))) in
      let typ = build_ast env depth ct in
      let match_typ = build_ast env depth m in
      let branches = List.map (build_ast env depth) (Array.to_list bs) in
      build_case ci typ match_typ branches
  | Fix ((is, i), (ns, ts, ds)) ->
      (debug_print "build_ast" "Fix";
      build_fix (build_fixpoint_functions env depth ns ts ds) i)
  | CoFix (i, (ns, ts, ds)) ->
      (debug_print "build_ast" "CoFix";
       build_cofix (build_fixpoint_functions env depth ns ts ds) i)
  | Proj (p, c) ->
      let _ = debug_print "build_ast" "Proj" in
      let p' = build_ast env depth (Term.mkConst (Projection.constant p)) in
      let c' = build_ast env depth c in
      build_proj p' c'

and build_const (env : Environ.env) (depth : int) ((c, u) : pconstant) =
  let kn = Constant.canonical c in
  let _ = debug_print "build_const" "Lookup 1" in
  let cd = Environ.lookup_constant c env in
  let _ = debug_print "build_const" "Lookup 2" in
  let global_env = Global.env () in
  match get_definition cd with
    None ->
      begin
        match cd.const_type with
          RegularArity ty -> build_axiom kn (build_ast global_env (depth - 1) ty) u
        | TemplateArity _ -> assert false (* pre-8.5 universe polymorphism *)
      end
  | Some c ->
      build_definition kn (build_ast global_env (depth - 1) c) u

and build_fixpoint_functions (env : Environ.env) (depth : int) (names : name array) (typs : constr array) (defs : constr array)  =
  let env_fix = Environ.push_rel_context (bindings_for_fix names typs) env in
  List.map
    (fun i ->
      let typ = build_ast env depth (Array.get typs i) in
      let def = build_ast env_fix depth (Array.get defs i) in
      build_fix_fun i (Array.get names i) typ def)
    (range 0 (Array.length names))

and build_oinductive (env : Environ.env) (depth : int) (ind_body : one_inductive_body) =
  let constrs =
    List.map
      (fun (i, (n, typ)) -> build (Names.string_of_id n) [i; build_ast env (depth - 1) typ])
    (named_constructors ind_body)
  in build (build "Name" [Names.string_of_id ind_body.mind_typename]) [build_inductive_body constrs]

and build_minductive (env : Environ.env) (depth : int) (((i, i_index), u) : pinductive) =
  let _ = debug_print "build_minductive" "lookup_mutind_body1" in
  let mutind_body = lookup_mutind_body i env in
  let ind_bodies = mutind_body.mind_packets in
  if depth <= 0 then (* don't expand *)
    build_inductive_name (Array.get ind_bodies i_index)
  else (* expand *)
    let ind_bodies_list = Array.to_list ind_bodies in
    let env_ind = Environ.push_rel_context (bindings_for_inductive env mutind_body ind_bodies_list) env in
    let cs = List.map (build_oinductive env_ind depth) ind_bodies_list in
    let ind_or_coind = mutind_body.mind_finite in
    build_inductive ind_or_coind cs u

(* --- build advanced rewriting term --- *)

let rec natFor (trm : types) =
  match kind_of_term trm with
  | Construct ((i, c_index), u) ->
      let (x,_) = i in
          if MutInd.to_string x="Coq.Init.Datatypes.nat" && c_index=1 then
              Some 0 else None
  | App (f, xs) ->
      (match kind_of_term f with
      | Construct ((i, c_index),u) ->
                  let (x,_) = i in
                      if MutInd.to_string x="Coq.Init.Datatypes.nat" && c_index=2 && Array.length xs==1 then
                          (match natFor (Array.get xs 0) with
                           | Some x -> Some (x+1)
                           | None -> None) else None
      | _ -> None)
  | _ -> None
  ;;

let rec build_type (env : Environ.env) (trm : types) =
  match kind_of_term trm with
  | Construct ((i, c_index), u) ->
      let (x,_) = i in
          Rtype.mkProduct (intern (MutInd.to_string x)) []
  | App (f, xs) ->
      (match kind_of_term f with
      | Construct ((i, c_index),u) ->
             let (x,_) = i in
             let xs' = List.map (build_type env) (Array.to_list xs) in
                 Rtype.mkProduct (intern (MutInd.to_string x)) xs'
      | _ -> Rtype.notype)
  | _ -> Rtype.notype

let rec build_exp (env : Environ.env) (trm : types) =
  match kind_of_term trm with
    Rel i ->
      debug_print "build_exp" "rel" ;
      build_rel_exp env i
  | Var v ->
      debug_print "build_exp" "var" ;
      build_var_exp v
  | Meta mv ->
      debug_print "build_exp" "meta" ;
      build_exp_meta mv
  | Evar (k, cs) ->
      debug_print "build_exp" "evar" ;
      let cs' = List.map (build_exp env) (Array.to_list cs) in
      build_evar_exp k cs'
  | Sort s ->
      debug_print "build_exp" "sort" ;
      build_sort_exp s
  | Cast (c, k, t) ->
      debug_print "build_exp" "cast";
      let c' = build_exp env c in
      let t' = build_exp env t in
      build_cast_exp c' k t'
  | Prod (n, t, b) ->
      debug_print "build_exp" "prod" ;
      let t' = build_exp env t in
      let b' = build_exp (Environ.push_rel (LocalAssum (n, t)) env) b in
      build_product_exp n t' b'
  | Lambda (n, t, b) ->
      debug_print "build_exp" "lambda" ;
      let t' = build_exp env t in
      let b' = build_exp (Environ.push_rel (LocalAssum (n, t)) env) b in
      build_lambda_exp n t' b'
  | LetIn (n, trm, typ, b) ->
      debug_print "build_exp" "letIn" ;
      let trm' = build_exp env trm in
      let typ' = build_exp env typ in
      let b' = build_exp (Environ.push_rel (LocalDef (n, b, typ)) env) b in
      LET (trm',Rtype.notype,typ',b')
  | App (f, xs) ->
      debug_print "build_exp" "App" ;
      (match natFor trm with
       | Some n -> (debug_print "build_exp" "Here1");NUM n
       | None -> (match build_app_constant_term env f xs with
                  | Some e -> (debug_print "build_exp" "Here2\n");e
                  | None ->
                    (debug_print "build_exp" "Here 3";match build_app_term env f xs with
                     | Some e -> (debug_print "build_exp" "Here 3a\n");e
                     | None ->
                        let f' = build_exp env f in
                        let xs' = List.map (build_exp env) (Array.to_list xs) in
                            (APPL (intern_apply,(f'::xs'))))))
  | Const (c, u) ->
      debug_print "build_exp" "Const" ;
      build_const_exp env (c, u)
  | Construct ((i, c_index), u) ->
      debug_print "build_exp" "Construct" ;
      (*let i' = build_exp env (Term.mkInd i) in*)
      (*let _ = print_string ("Constr result " ^ (prExp i') ^ "\n") in*)
      let (x,_) = i in
      let s = (MutInd.to_string x) in
      let _ = debug_print "build_exp" ("s = "^s^" i = "^(string_of_int c_index)^"\n") in
          if s="Coq.Init.Datatypes.nat" && c_index=1 then
              NUM 0
          else if s="Coq.Init.Datatypes.bool" && c_index=1 then
              (APPL (intern_true,[]))
          else if s="Coq.Init.Datatypes.bool" && c_index=2 then
              (APPL (intern_false,[]))
          else if s="Coq.Init.Datatypes.list" && c_index=1 then
              (APPL (intern_nil,[]))
          else
              (APPL ((intern (("C_"^s^" "^(string_of_int c_index)))),[]))
  | Ind ((i, i_index), u) ->
      debug_print "build_exp" "Ind\n" ;
      (match build_inductive_term env i i_index with
       | Some x -> x
       | None -> build_minductive_exp env ((i, i_index), u))
  | Case (ci, ct, m, bs) ->
      debug_print "build_exp" "Case";
      let _ = (case_infos := (ci::(!case_infos))) in
      let typ = build_exp env ct in
      let match_typ = build_exp env m in
      let branches = List.map (build_exp env) (Array.to_list bs) in
      build_case_exp ci typ match_typ branches
  | Fix ((is, i), (ns, ts, ds)) ->
      debug_print "build_exp" "Fix";
      build_fix_exp (build_fixpoint_functions_exp env ns ts ds) i
  | CoFix (i, (ns, ts, ds)) ->
      debug_print "build_exp" "CoFix";
      build_cofix_exp (build_fixpoint_functions_exp env ns ts ds) i
  | Proj (p, c) ->
      debug_print "build_exp" "Proj";
      let p' = build_exp env (Term.mkConst (Projection.constant p)) in
      let c' = build_exp env c in
      build_proj_exp p' c'
and build_inductive_term (env : Environ.env) i i_index =
    Some (match ((MutInd.to_string i),i_index) with
          | ("Coq.Init.Logic.True",_) -> (APPL (intern_true,[]))
          | ("Coq.Init.Logic.False",_) -> (APPL (intern_false,[]))
          | (x,_) -> (APPL (intern ("C_" ^ x),[])))
and build_app_term (env : Environ.env) f xs =
      ((*print "Here 0\n";*)
       match kind_of_term f with
      | Ind ((i, c_index),u) ->
             let xs' = List.map (build_exp env) (Array.to_list xs) in
             let _ = debug_print "arewrite" ("Ind s = "^(MutInd.to_string i)) in
                 Some (match MutInd.to_string i with
                       | "Coq.Init.Logic.or" -> (APPL (intern_or,xs'))
                       | "Coq.Init.Logic.and" -> (APPL (intern_and,xs'))
                       | "Coq.Init.Logic.not" -> (APPL (intern_not,xs'))
                       | "Coq.Init.Logic.ex" ->
                              (match xs' with
                              | [t1;(QUANT (intern_lambda,[(v1,t2)],b,_))] ->
                                    (QUANT (intern_exists,[(v1,t2)],b,(APPL (intern_true,[]))))
                              | _ -> (APPL ((intern "Coq.Init.Logic.ex"),xs')))
                       | "Coq.Init.Logic.eq" -> (if (List.length xs')=3 then APPL (intern_equal,[(List.nth xs' 1);(List.nth xs' 2)]) else APPL (intern ("C_Coq.Init.Logic.eq"),xs'))
                       | x -> (APPL ((intern ("C_" ^ x),xs'))))
      | Const (c, u) ->
            let kn = Constant.canonical c in
            let kn' = build_kername kn in
            let xs' = List.map (build_exp env) (Array.to_list xs) in
                 Some (match kn' with
                       | "Coq.Init.Datatypes.andb" -> (APPL (intern_and,xs'))
                       | "Coq.Init.Datatypes.orb" -> (APPL (intern_or,xs'))
                       | "Coq.Init.Datatypes.negb" -> (APPL (intern_not,xs'))
                       | "Coq.Init.Nat.add" -> (APPL (intern_nat_plus,xs'))
                       | "Coq.Init.Nat.sub" -> (APPL (intern_nat_minus,xs'))
                       | "Coq.Init.Nat.mul" -> (APPL (intern_nat_times,xs'))
                       | "Coq.Init.Peano.lt" -> (APPL (intern_nat_less,xs'))
                       | "Coq.Init.Logic.eq" -> (APPL (intern_equal,xs'))
                       | "Coq.Init.Logic.not" -> (APPL (intern_not,xs'))
                       | x -> if (String.length kn')>3 &&
                                 (String.sub kn' 0 3)=="Coq" then
                                 (APPL (intern x,xs'))
                              else
                              (let d = try get_def kn' with NoEntry ->
                               let cd = Environ.lookup_constant c env in
                               let global_env = Global.env () in
                               let dd =
                                   match get_definition cd with
                                     None ->
                                       begin
                                         match cd.const_type with
                                           RegularArity ty -> ty (*build_axiom_exp kn (build_exp global_env ty) u*)
                                         | TemplateArity _ -> assert false (* pre-8.5 universe polymorphism *)
                                       end
                                   | Some c ->
                                       (build_definition_exp kn (build_exp global_env c) u);c
                               in ((add_def kn' dd);dd) in
                                                   debug_print "build_exp" ("building "^("f_"^x));APPL (intern ("f_" ^ x),xs')))
      | Construct ((i, c_index),u) ->
             let (x,_) = i in
             (*let _ = print ("s = "^(MutInd.to_string x)^" i = "^(string_of_int c_index)^"\n") in*)
                 if (MutInd.to_string x)="Coq.Init.Datatypes.list" && c_index=2 then
                     Some (APPL (intern_cons,[build_exp env (Array.get xs 1);build_exp env (Array.get xs 2)]))
                 else if (MutInd.to_string x)="Coq.Init.Logic.not" then
                     Some (APPL (intern_not,[build_exp env (Array.get xs 1)]))
                 else
                     None
      | _ -> None)
and build_app_constant_term (env : Environ.env) f xs =
      (match kind_of_term f with
      | Construct ((i, c_index),u) ->
             let (x,_) = i in
             let xs' = List.map (build_exp env) (Array.to_list xs) in
             let _ = (add_constr ("C_" ^ (MutInd.to_string x) ^ " " ^ (string_of_int c_index)) f) in
                 if (MutInd.to_string x)="Coq.Init.Datatypes.list" && c_index=2 then
                     Some (APPL (intern_cons,[build_exp env (Array.get xs 1);build_exp env (Array.get xs 2)]))
                 else if (MutInd.to_string x)="Coq.Init.Datatypes.list" && c_index=1 then
                     Some (APPL (intern_nil,[]))
                 else
                     Some (APPL ((intern ("C_" ^ (MutInd.to_string x)^" "^(string_of_int c_index))),xs'))
      | _ -> None)
and build_const_exp (env : Environ.env) ((c, u) : pconstant) =
  let kn = Constant.canonical c in
  let kn' = build_kername kn in
      if (String.length kn')>3 &&
         (String.sub kn' 0 3)=="Coq" then
         (APPL (intern kn',[]))
      else let d = try get_def kn' with NoEntry ->
          let cd = Environ.lookup_constant c env in
          let global_env = Global.env () in
          let dd =
              match get_definition cd with
                None ->
                  begin
                    match cd.const_type with
                      RegularArity ty -> ty (*build_axiom_exp kn (build_exp global_env ty) u*)
                    | TemplateArity _ -> assert false (* pre-8.5 universe polymorphism *)
                  end
              | Some c ->
                  (build_definition_exp kn (build_exp global_env c) u);c
          in ((add_def kn' dd);dd) in
          (debug_print "build_const_exp" ("building2 "^("f_"^kn'));APPL (intern ("f_" ^ kn'),[]))

and build_fixpoint_functions_exp (env : Environ.env) (names : name array) (typs : constr array) (defs : constr array)  =
  let env_fix = Environ.push_rel_context (bindings_for_fix names typs) env in
  List.map
    (fun i ->
      let typ = build_exp env (Array.get typs i) in
      let def = build_exp env_fix (Array.get defs i) in
      build_fix_fun_exp i (Array.get names i) typ def)
    (range 0 (Array.length names))

and build_oinductive_exp (env : Environ.env) (ind_body : one_inductive_body) =
  let constrs =
    List.map
      (fun (i, (n, typ)) -> APPL ((intern (Names.string_of_id n)),[APPL ((intern i),[]); build_exp env typ]))
    (named_constructors ind_body)
  in APPL ((intern ("Name "^(Names.string_of_id ind_body.mind_typename))),[build_inductive_body_exp constrs])

and build_minductive_exp (env : Environ.env) (((i, i_index), u) : pinductive) =
  let _ = debug_print "build_minductive_exp" "lookup_mutind_body2" in
  let mutind_body = lookup_mutind_body i env in
  let ind_bodies = mutind_body.mind_packets in
    let ind_bodies_list = Array.to_list ind_bodies in
    let env_ind = Environ.push_rel_context (bindings_for_inductive env mutind_body ind_bodies_list) env in
    let cs = List.map (build_oinductive_exp env_ind) ind_bodies_list in
    let ind_or_coind = mutind_body.mind_finite in
    build_inductive_exp ind_or_coind cs u

(* --- Top-level functionality --- *)

(*
 * Apply a function to a definition up to a certain depth
 * That is, always unfold the first constant or inductive definition
 *)
let apply_to_definition (f : Environ.env -> int -> types -> 'a) (env : Environ.env) (depth : int) (body : types) =
  match (kind_of_term body) with
  | Const _ ->
      f env (depth + 1) body
  | Ind _ ->
      f env (depth + 1) body
  | _ ->
      f env depth body

(* Top-level print AST functionality *)
let print_ast (depth : int) (def : Constrexpr.constr_expr) =
  let (evm, env) = Lemmas.get_current_context() in
  let (body, _) = Constrintern.interp_constr env evm def in
  let ast = apply_to_definition build_ast env depth body in
  print ast

let debug_print_ast f d =
    if (List.mem "ALL" debug_ids) || (List.mem f debug_ids) then
        let ast = apply_to_definition build_ast (Global.env ()) 0 d in
        let _ = debug_print f ast in () else ()

(* Top-level print AST functionality *)
let print_exp (depth : int) (def : Constrexpr.constr_expr) =
  let (evm, env) = Lemmas.get_current_context() in
  let (body, _) = Constrintern.interp_constr env evm def in
  let ast = prExp (Renv.flatten Renv.emptyEnv (build_exp env body)) in
  print ast

(*let rec buildResult exp =*)

let reify_cons = lazy (Lib_coq.init_constant ["Coq" ; "Init"; "Datatypes"] "cons")
let reify_nil = lazy (Lib_coq.init_constant ["Coq" ; "Init"; "Datatypes"] "nil")
let reify_nat = lazy (Lib_coq.init_constant ["Coq" ; "Init"; "Datatypes"] "nat")
let reify_bool = lazy (Lib_coq.init_constant ["Coq" ; "Init"; "Datatypes"] "bool")
let reify_eq = lazy (Lib_coq.init_constant ["Coq";"Init";"Logic"] "eq")
let reify_ex = lazy (Lib_coq.init_constant ["Coq";"Init";"Logic"] "ex")
let reify_eq_val = lazy (Lib_coq.init_constant ["Coq";"Bool";"BoolEq"] "beq")
let reify_lt = lazy (Lib_coq.init_constant ["Coq";"Init";"Peano"] "lt")
let reify_lt_val = lazy (Lib_coq.init_constant ["Coq";"Init";"Nat"] "ltb")
let reify_true = lazy (Lib_coq.init_constant ["Coq";"Init";"Logic"] "True")
let reify_false = lazy (Lib_coq.init_constant ["Coq";"Init";"Logic"] "False")
let reify_true_val = lazy (Lib_coq.init_constant ["Coq";"Init";"Datatypes"] "true")
let reify_false_val = lazy (Lib_coq.init_constant ["Coq";"Init";"Datatypes"] "false")
let reify_and = lazy (Lib_coq.init_constant ["Coq";"Init";"Logic"] "and")
let reify_or = lazy (Lib_coq.init_constant ["Coq";"Init";"Logic"] "or")
let reify_and_val = lazy (Lib_coq.init_constant ["Coq";"Init";"Datatypes"] "andb")
let reify_or_val = lazy (Lib_coq.init_constant ["Coq";"Init";"Datatypes"] "orb")
let reify_not_val = lazy (Lib_coq.init_constant ["Coq";"Init";"Datatypes"] "negb")
let reify_imply_val = lazy (Lib_coq.init_constant ["Coq";"Init";"Datatypes"] "implb")
let reify_add = lazy (Lib_coq.init_constant ["Coq";"Init";"Nat"] "add")
let reify_mul = lazy (Lib_coq.init_constant ["Coq";"Init";"Nat"] "mul")
let reify_sub = lazy (Lib_coq.init_constant ["Coq";"Init";"Nat"] "sub")
let reify_div = lazy (Lib_coq.init_constant ["Coq";"Init";"Nat"] "div")

let test_term = Term.mkApp (Lazy.force reify_eq, [|(Lazy.force reify_nat);Lib_coq.Nat.of_int 2;Lib_coq.Nat.of_int 3|])

(*let rec build_term x = Lib_coq.Nat.of_int 42 ;;*)

exception NoTypeInfo;;

let root_name s =
    let st = Intern.decode s in
    if String.length st > 3 then
        let s1 = String.sub st 2 ((String.length st)-2) in
        let sl = String.split_on_char ' ' s1 in
            if (List.length sl)>0 then
                List.hd sl
            else
                ""
    else
        ""

let root_index s =
    let st = Intern.decode s in
    if String.length st > 3 then
        let s1 = String.sub st 2 ((String.length st)-2) in
        let sl = String.split_on_char ' ' s1 in
            if (List.length sl)=2 then
                int_of_string (List.hd (List.tl sl))
            else
                -1
    else
        -1

let print_basename sp = pr_global (ConstRef sp)

let ungeneralized_type_of_constant_type t =
  Typeops.type_of_constant_type (Global.env ()) t

let ac = intern "f_AdvancedRewrite.advancedRewrite.AC"
let rewrite_rule = intern "f_AdvancedRewrite.advancedRewrite.REWRITE_RULE"

let rec root_prop p = match p with
    | (QUANT (q,v,e,p)) -> root_prop e
    | x -> x
    ;;

let process_property env p =
    (debug_print "process_property" ("Property " ^ (prExp p));
     debug_print "process_property" ("Root Property " ^ (prExp (root_prop p)));
    match (root_prop p) with
    | (APPL (f,[_;t])) -> if f=ac then
                              match t with
                              | (APPL (f,_)) ->
                                  let d = decode f in
                                      if (String.length d)>4 then
                                          let _ = debug_print "process_property" ("Adding ac " ^ d) in
                                              Renv.addAttrib env intern_ac [Renv.S(f)]
                                          else env
                              | _ -> env
                          else env
    | (APPL (f,[_;l;r;c])) -> if f=rewrite_rule then
                                  (debug_print "process_property" ("Adding rule "^(prExp (APPL (intern_oriented_rule,[l;r;c]))));
                                  Renv.addProperty env (APPL (intern_oriented_rule,[l;r;c])))
                              else env
    | _ -> env) ;;

let build_terms f =
    let sf = String.split_on_char '.' (decode f) in
        if List.length sf==2 && (List.hd sf)="f_Top" then
            [(intern (List.hd (List.tl sf)))]
        else []

let rec get_syms e =
    match e with
    | (APPL (f,l)) -> (List.fold_left List.append (build_terms f) (List.map get_syms l))
    | (CASE (v,t,c)) -> (List.fold_left List.append (get_syms v) (List.map (fun (p,e) -> (List.append (get_syms p) (get_syms e))) c))
    | (QUANT (n,v,e,p)) -> List.append (get_syms e) (get_syms p)
    | _ -> [] ;;


let name_away_from_vars v (p,e) =
    let fv = Rcontext.getFreeVars p in
    let rec build_sub v fv = match fv with
                             | [] -> Rsubst.empty
                             | (vv::r) ->
                               let nv = (Rcontext.name_away_from_list v vv) in
                                   Rsubst.addPair
                                   (build_sub (nv::v) r) vv (VAR nv) in
    let sub = build_sub v fv in
        (Rsubst.subst sub p,Rsubst.subst sub e)

let rec sub_vars_list vars v pat =
    match vars with
    | [] -> []
    | (f::r) -> let u = Rsubst.addPair Rsubst.empty v pat in
                    (Rsubst.subst u f)::(sub_vars_list r v pat)

let rec remove_case (vars : Exp.exp list) (cond : Exp.exp) (core : Exp.exp) locals =
    match core with
    | (CASE ((VAR v),ttt,c)) ->
        if List.mem v locals then
            let rec sub_remove_case h rest =
                match rest with
                | [] -> None
                | ((c,t)::r) -> match (remove_case vars cond t (List.append (Rcontext.getFreeVars c) locals)) with
                            | Some l -> Some (List.map (fun x ->
                                                match x with
                                                | (vv,cc,e) ->
                                                  (vv,cc,(CASE ((VAR v),ttt,(List.append h ((c,e)::r)))))
                                                | z -> z) l)
                            | None -> sub_remove_case (List.append h [(c,t)]) r in
            sub_remove_case [] c
        else
            let rec fv_list vl = match vl with
                                 | [] -> []
                                 | (a::b) -> List.append
                                             (Rcontext.getFreeVars a)
                                             (fv_list b) in
            let fv = List.append locals (fv_list vars) in
            let cl = List.map (fun (p,e) ->
                                let (p',e') = name_away_from_vars fv (p,e) in
                                let vars' = sub_vars_list vars v p' in
                                    (vars',cond,e')
                              ) c in Some cl
    | (CASE (v,ttt,c)) ->
       (match remove_case vars cond v locals with
        | Some l -> Some (List.map (fun (v,cc,e) -> (v,cc,(CASE (e,ttt,c)))) l)
        | None ->
            let rec sub_remove_case h rest =
                match rest with
                | [] -> None
                | ((c,t)::r) -> match (remove_case vars cond t (List.append (Rcontext.getFreeVars c) locals)) with
                            | Some l -> Some (List.map (fun x ->
                                                match x with
                                                | (vv,cc,e) ->
                                                  (vv,cc,(CASE (v,ttt,(List.append h ((c,e)::r)))))
                                                | z -> z) l)
                            | None -> sub_remove_case (List.append h [(c,t)]) r in
            sub_remove_case [] c)
    | (APPL (18,[c;l;r])) ->
      Some [(vars,(APPL (intern_and,[c;cond])),l);
            (vars,(APPL (intern_and,[(APPL (intern_not,[c]));cond])),r)]
    | (APPL (ff,l)) ->
      let rec sub_remove_case h rest =
              match rest with
              | [] -> None
              | (f::r) -> match (remove_case vars cond f locals) with
                          | Some l -> Some (List.map (fun x ->
                                                match x with
                                                | (v,c,e) ->
                                                  (v,c,(APPL (ff,(List.append h (e::r)))))
                                                | z -> z) l)
                          | None -> sub_remove_case (List.append h [f]) r in
          sub_remove_case [] l
    | _ -> None ;;

let rec build_rules dname vars cond core =
    let l = remove_case vars cond core [] in
        match l with
        | None -> [(APPL (intern_oriented_rule,[APPL (dname,vars);core;cond]))]
        | Some l -> List.fold_left List.append []
                        (List.map (fun (vars,cond,core) ->
                                        build_rules dname vars cond core) l)

let process_var_core env dname vars core =
    let syms = get_syms core in
    let d = intern ("f_Top." ^ dname) in
    let _ = debug_print "process_var_core" ("The syms of " ^ dname ^ " " ^ (prExp core)) in
    let _ = List.map (fun x -> debug_print "process_var_core" ("Sym: " ^ (decode x) ^ "\n")) syms in
    let env1 = List.fold_left (fun e -> fun s ->
                   let c = intern ("f_Top." ^ (decode s)) in
                       Renv.addPrecedence e (d,c)) env syms in
    let rules = build_rules (intern dname) (List.map (fun x -> (VAR x)) vars) (APPL (intern_true,[])) core in
    let _ = List.map (fun x -> debug_print "process_var_core" ("Rule: " ^ (prExp x))) rules in
        env1

let rec fun_break l x = match x with
  | (QUANT (73,[(v,t)],e,c)) -> fun_break (List.append l [v]) e
  | x -> (l,x) ;;


let rec core_fix core dname full_name =
    match core with
    | (APPL (75,(VAR v::r))) ->
          let r1 = List.map (fun x -> core_fix x dname full_name) r in
              if v=dname then
                  (APPL (full_name,r1))
              else
                  (APPL (75,(VAR v::r1)))
    | (APPL (f,l)) -> (APPL (f,(List.map (fun x -> core_fix x dname full_name) l)))
    | (CASE (v,t,c)) -> (CASE ((core_fix v dname full_name),t,
                               (List.map (fun (p,e) -> (p,core_fix e dname full_name)) c)))
    | x -> x
    ;;

let process_def env d =
    (debug_print "process_def" ("process_def " ^ (prExp d)));match d with
    | (APPL (91,[(APPL (fns,[APPL (fname,[n;t;dd])]));nn])) ->
      (debug_print ("**DEF** " ^ (decode fname) ^ " " ^ (prExp dd));
      let ff = String.split_on_char ' ' (decode fname) in
          if List.length ff = 2 then
              let q = List.hd (List.tl ff) in
              let name = String.sub q 0 ((String.length q)-1) in
              let (vars,core) = fun_break [] dd in
              let core = core_fix core (intern name) (intern ("f_Top."^name)) in
                 (debug_print "process_def" ("Name: " ^ name ^ "\nCore: " ^ (prExp core)));process_var_core env ("f_Top."^name) vars core
          else env)
    | _ -> env ;;

let build_rewrite_env env =
  let rec prec = function
  | (env,((_,kn),Lib.Leaf lobj)::rest) ->
      let env = match object_tag lobj with
      | "CONSTANT" ->
          let _ = debug_print "build_rewrite_env" "HERE build_rewrite_env" in
          let con = Global.constant_of_delta_kn kn in
          let cb = Global.lookup_constant con in
          let typ = ungeneralized_type_of_constant_type cb.const_type in
          (*hov 0*) (
            match cb.const_body with
              | Undef _ -> (debug_print "build_rewrite_env" ("Parsed " ^ prExp (build_exp (Global.env ()) typ));process_property env (build_exp (Global.env ()) typ))
                (*str "Parameter " ++
                print_basename con ++ str " : " ++ cut () ++ pr_ltype typ*)
              | OpaqueDef lc ->
                (*str "Theorem " ++ print_basename con ++ cut () ++
                str " : " ++ pr_ltype typ ++ str "." ++ fnl () ++
                str "Proof " ++ pr_lconstr (Opaqueproof.force_proof (Global.opaque_tables ()) lc)*) env
              | Def c ->
                (*str "Definition " ++ print_basename con ++ cut () ++
                str "  : " ++ pr_ltype typ ++ cut () ++ str " := " ++
                pr_lconstr (Mod_subst.force_constr c)*) (debug_print "build_rewrite_env" ("DEF " ^ (prExp (build_exp (Global.env ()) (Mod_subst.force_constr c))));
((debug_print "build_rewrite_env" "AST: ");debug_print "build_rewrite_env" (build_ast (Global.env ()) 0 (Mod_subst.force_constr c)));
(process_def env (build_exp (Global.env ()) (Mod_subst.force_constr c)))
))
      | x -> (debug_print "build_rewrite_env" ("build_rewrite_env UNKNOWN " ^ x);env) in
      prec (env,rest)
  | (env,(f::r)) -> prec (env,r)
  | (env,_) -> env in
  prec (env,(Lib.contents ()))

let print_full_pure_context () =
  let rec prec = function
  | ((_,kn),Lib.Leaf lobj)::rest ->
      let pp = match object_tag lobj with
      | "CONSTANT" ->
          let con = Global.constant_of_delta_kn kn in
          let _ = debug_print "print_full_pure_context" "Lookup 3" in
          let cb = Global.lookup_constant con in
          let _ = debug_print "print_full_pure_context" "Lookup 4" in
          let typ = ungeneralized_type_of_constant_type cb.const_type in
          hov 0 (
            match cb.const_body with
              | Undef _ ->
                str "Parameter " ++
                print_basename con ++ str " : " ++ cut () ++ pr_ltype typ
              | OpaqueDef lc ->
                str "Theorem " ++ print_basename con ++ cut () ++
                str " : " ++ pr_ltype typ ++ str "." ++ fnl () ++
                str "Proof " ++ pr_lconstr (Opaqueproof.force_proof (Global.opaque_tables ()) lc)
              | Def c ->
                str "Definition " ++ print_basename con ++ cut () ++
                str "  : " ++ pr_ltype typ ++ cut () ++ str " := " ++
                pr_lconstr (Mod_subst.force_constr c))
          ++ str "." ++ fnl () ++ fnl ()
      | "INDUCTIVE" ->
          let mind = Global.mind_of_delta_kn kn in
          let mib = Global.lookup_mind mind in
          pr_mutual_inductive_body (Global.env()) mind mib ++
            str "." ++ fnl () ++ fnl ()
      | "MODULE" ->
          (* TODO: make it reparsable *)
          let (mp,_,l) = repr_kn kn in
          print_module true (MPdot (mp,l)) ++ str "." ++ fnl () ++ fnl ()
      | "MODULE TYPE" ->
          (* TODO: make it reparsable *)
          (* TODO: make it reparsable *)
          let (mp,_,l) = repr_kn kn in
          print_modtype (MPdot (mp,l)) ++ str "." ++ fnl () ++ fnl ()
      | x -> (str "Unknown ") ++ (str x) ++ str " " ++ str (KerName.debug_to_string kn) ++ str "\n" ++ cut () ++ (mt ()) in
      prec rest ++ pp
  | ((_,kn),(Lib.CompilingLibrary (d,(m,d2))))::rest -> (str "Compiling Library ") ++ (str (DirPath.to_string d)) ++ (str " ") ++ (str (ModPath.to_string m)) ++ (str " ") ++ (str (DirPath.to_string d2)) ++ (str "\n") ++ prec rest
  | ((_,kn),(Lib.OpenedModule (a,b,c,d)))::rest -> (str "Opened module\n") ++ prec rest
  | ((_,kn),(Lib.ClosedModule ls))::rest -> (str "Closed module\n") ++ prec rest
  | ((_,kn),(Lib.OpenedSection (a,b)))::rest -> (str "Opened section\n") ++ prec rest
  | ((_,kn),(Lib.ClosedSection ls))::rest -> (str "Closed section\n") ++ prec rest
  | _ -> mt () in
  prec (Lib.contents ())

let rec root_type l t =
    if l=0 then t else
    match kind_of_term t with
    | Prod (n, t, b) -> root_type (l-1) b
    | x -> t
    ;;

let type_from_name s =
    let _ = debug_print "type_from_name" (decode s) in
    let (evm, env) = Lemmas.get_current_context() in
    try
        let Construct (m,u) = kind_of_term (get_constr (decode s)) in
        (*let xx = Global.lookup_mind (Construct (m,u)) in*)
        let root = root_name s in
        let index = root_index s in
        let rec sl h r = match r with
                         | [x] -> (h,x)
                         | [] -> (h,"")
                         | (f::r) -> sl (List.append h [f]) r
                         in
        let _ = debug_print "type_from_name" ("root " ^ root) in
        let (path,name) = sl [] (String.split_on_char '.' root) in
        let dirpath = DirPath.make (List.map Id.of_string path) in
        let modpath = ModPath.MPfile dirpath in
        let d = Environ.lookup_mind (MutInd.make2 modpath (Names.Label.make name)) env in
        let _ = debug_print "type_from_name" "Here 3" in
        (*let (body, _) = Constrintern.interp_constr env evm def in*)
        let ind_bodies = d.mind_packets in
        let ind_bodies_list = Array.to_list ind_bodies in
        let env_ind = Environ.push_rel_context (bindings_for_inductive env d ind_bodies_list) env in
        let cs = List.map (build_oinductive env_ind 0) ind_bodies_list in
        let ind_or_coind = d.mind_finite in
        let ast = build_inductive ind_or_coind cs u in
        let _ = debug_print "type_from_name" "Here 4" in
        let _ = debug_print "type_from_name" ast in
            Lazy.force reify_nat
    with NoEntry ->
        let d = get_def (String.sub (decode s) 2 ((String.length (decode s))-2)) in
        let _ = debug_print_ast "type_from_name" d in
        let td = match kind_of_term d with
                 | Fix ((is, i), (ns, ts, ds)) -> ts
                 | _ -> debug_print "type_from_name" "Failed3";raise NoEntry in
        let ast2 = Array.map (apply_to_definition build_ast env 0) td in
        let _ = Array.map (debug_print "type_from_name") ast2 in
        let t = Array.get td 0 in
        let rt = root_type (-1) t in
        let _ = debug_print "type_from_name" "Done" in rt

        (*let x = Lib_coq.init_constant root in
        let kn = Constant.canonical c in
        let _ = debug_print "build_const" "Lookup 1" in
        let cd = Environ.lookup_constant c env in
        let _ = debug_print "build_const" "Lookup 2" in
        let global_env = Global.env () in
        match get_definition cd with
          None -> debug_print "type_from_name" "Failed4";raise NoEntry
        | Some c ->
            build_definition kn (build_ast global_env (depth - 1) c) u*)

let functor_from_name s =
    let _ = debug_print "functor_from_name" ("functor_from_name " ^ (decode s)) in
    let root = root_name s in
    let _ = debug_print "functor_from_name" ("root " ^ root) in
    let index = root_index s in
    let rec sl h r = match r with
                     | [x] -> (h,x)
                     | [] -> (h,"")
                     | (f::r) -> sl (List.append h [f]) r
                     in
    let (path,name) = sl [] (String.split_on_char '.' root) in
    try
        let d = (get_constr (decode s)) in
        let (evm, env) = Lemmas.get_current_context() in
        (*let (body, _) = Constrintern.interp_constr env evm def in*)
        let _ = debug_print_ast "functor_from_name" d in
            d
    with NoEntry ->
        (debug_print "functor_from_name" ("initing " ^ name ^ " " ^ (string_of_int index));
        if index < 0 then
            Lib_coq.init_constant path name
        else
            let (evm, env) = Lemmas.get_current_context() in
            (*let x = Lib_coq.init_constant path name in*)
            let coq_init_specif = ModPath.MPfile (DirPath.make (List.map Id.of_string (List.rev path))) in
            let _ = debug_print "functor_from_name" "Making inductive term" in
            let sigT = mkInd (MutInd.make1 (KerName.make2 coq_init_specif (Label.make name)), 0) in
            let _ = debug_print "functor_from_name" "Done making inductive term" in
            let term = mkConstruct (fst (destInd sigT), index) in
            let _ = debug_print_ast "functor_from_name" term in
            let _ = debug_print "functor_from_name" "DONE\n" in term)

let rec build_var v tenv = match tenv with
  | ((vv,t,n)::r) -> if v=vv then mkRel n else build_var v r
  | _ -> mkVar (Id.of_string v)

let rec push_var v t tenv = (*((v,t,(List.length tenv)+1)::tenv)*)
  match tenv with
  | ((vv,tp,n)::r) -> ((vv,tp,n+1)::(push_var v t r))
  | _ -> [(v,t,1)]

let build_leaf_type t = ((debug_print "build_leaf_type" ("build leaf " ^ (decode t)));match (decode t) with
  | "Natural" -> Lazy.force reify_nat
  | "C_Coq.Init.Datatypes.nat" -> Lazy.force reify_nat
  | "Bool" -> Lazy.force reify_bool
  | x -> (debug_print "build_leaf_type" ("Initing22 "^x^"\n");
         (if (String.length x) > 2 && (((String.sub x 0 2)="C_") || ((String.sub x 0 2)="f_")) then
             functor_from_name (Intern.intern x)
         else
             functor_from_name (Intern.intern ("f_"^x)))))

let rec build_coq_type t =
  try (debug_print "build_coq_type" ("HEREprod "^(Rtype.unparse t));let tl = Rtype.paramProduct t in
  match tl with
  | [] -> build_leaf_type (Rtype.nameProduct t)
  | l -> let tl = List.map build_coq_type l in
         let r = Term.mkApp((build_leaf_type (Rtype.nameProduct t)),Array.of_list tl) in r) with (Rtype.TypeError(_)) ->
    let _ = debug_print "build_coq_type" ("CAUGHT "^(Rtype.unparse t)^"\n") in
    let (t1,t2) = Rtype.untypeTfun t in
    let tt1 = build_coq_type t1 in
    let tt2 = build_coq_type t2 in
    let r = Term.mkProd(Anonymous,tt1,tt2) in r

let rec get_var_type tenv v = match tenv with
  | ((vv,t,n)::r) -> if v=vv then (build_coq_type t) else get_var_type r v
  | _ -> raise NoTypeInfo ;;

let rec get_type x tenv = match x with
  | (NUM _) -> (Lazy.force reify_nat)
  | (APPL (4,[])) -> (Lazy.force reify_bool)
  | (APPL (5,[])) -> (Lazy.force reify_bool)
  | (APPL (11,_)) -> (Lazy.force reify_bool)
  | (APPL (12,_)) -> (Lazy.force reify_bool)
  | (APPL (17,_)) -> (Lazy.force reify_bool)
  | (APPL (18,[c;l;r])) -> (try debug_print "get_type" "Recursive call";get_type l tenv with NoTypeInfo -> get_type r tenv)
  | (APPL (76,_)) -> (Lazy.force reify_nat)
  | (APPL (77,_)) -> (Lazy.force reify_nat)
  | (APPL (78,_)) -> (Lazy.force reify_nat)
  | (APPL (79,_)) -> (Lazy.force reify_nat)
  | (APPL (80,_)) -> (Lazy.force reify_bool)
  | (APPL (86,_)) -> (Lazy.force reify_bool)
  | (APPL (90,_)) -> (Lazy.force reify_bool)
  | (APPL (f,_)) -> debug_print "get_type" "type_from_name call";(try type_from_name f with NoEntry -> raise NoTypeInfo)
  | (VAR x) -> get_var_type tenv (decode x)
  | (CASE (e,t,c)) -> get_case_type c tenv
  | _ -> raise NoTypeInfo
and get_case_type c tenv = match c with
  | [] -> raise NoTypeInfo
  | ((p,e)::r) -> try debug_print "get_type" "recursive call";get_type e tenv with NoTypeInfo -> get_case_type r tenv
  ;;

let is_bool_type x tenv =
   try
       let _ = debug_print "is_bool_type" "Called" in
       let typ = EConstr.Unsafe.to_constr (Typing.unsafe_type_of (Global.env()) (Evd.empty) (EConstr.of_constr x)) in
       let _ = debug_print "is_bool_type" "Called1" in
       let r = match (kind_of_term typ) with
               | Ind ((i, i_index), u) -> (debug_print "is_bool_type" ("Called2 "^(MutInd.debug_to_string i));"(Coq.Init.Datatypes.bool)"=(MutInd.debug_to_string i))
               | x -> false in r
    with NoEntry -> false

let constructorList t =
    let name = decode (Rtype.nameProduct t) in
    match name with
    | "Natural" -> [(intern "Z",[]);(intern "S",[t])]
    | "Bool" -> [(intern_true,[]);(intern_false,[])]
    | "List" -> ([(intern "Nil",[]);(intern_cons,[List.hd (Rtype.paramProduct t);t])])
    | _ -> []

let buildCase t constructor cases =
    let rec name_away_vars e l = match l with
        | [] -> []
        | t::r -> let v = Rcontext.name_away e (intern "v") in
                  v::(name_away_vars (APPL (intern_and,[VAR v;e])) r) in
    let rec getRelevant c l cases =
        match cases with
        | []-> []
        | (((APPL (x,e)),t)::r) ->
          if c=x then ((e,t)::getRelevant c l r) else getRelevant c l r
        | ((VAR x,e)::r) ->
          let r' = List.map (fun x -> VAR x) (name_away_vars e l) in
              (r',Rsubst.subst (Rsubst.addPair Rsubst.empty x (APPL (c,r'))) e)::(getRelevant c l r)
        | (_::r) -> getRelevant c l r in
    let rec all_vars l = match l with
                     | [] -> true
                     | ((VAR _)::r) -> all_vars r
                     | _ -> false in
    let rec get_types cl = match cl with
      | ((n,t)::r) -> if n=constructor then t else get_types r in
    let types = get_types (constructorList t) in
    let rec build_lambda l tl e = match (l,tl) with
      | ([],[]) -> e
      | ((VAR v)::r1,t::r2) -> (QUANT (intern_lambda,[(v,t)],build_lambda r1 r2 e,(APPL (intern_true,[])))) in
        let rl = getRelevant constructor types cases in
            if List.length rl=1 then
               (let (v,t) = List.hd rl in
                   if all_vars v then
                       (build_lambda v types t)
                   else (APPL (intern_undef,[])))
            else (APPL (intern_undef,[]))

(*let buildConstructors (t : Rtype.etype) (branch_asts :  constr list) =
    let name = decode (Rtype.nameProduct t) in
    let rec bc e n l = match e with
                     | (QUANT (intern_lambda,([(v,t)]),ex,te)) ->
                       bc ex n (l@[v])
                     | e -> ((APPL (intern (name ^ (string_of_int n)),List.map (fun x -> VAR x) l)),e) in
    let rec ge e = match e with
                   | (QUANT (intern_lambda,([(v,t)]),ex,te)) ->
                     ge ex
                   | e -> e in
    let rec bf bl n = match bl with
                    | [] -> []
                    | (f::r) -> (bc f n [])::(bf r (n+1)) in
        if name="Natural" then
            let (QUANT (intern_lambda,([(v,t)]),ex,te)) = List.nth branch_asts 1 in
                [((NUM 0),(List.nth branch_asts 0));((APPL (intern "S",[VAR (intern "n")])),ex)]
        else if name="Bool" then
            [((APPL (intern_true,[])),(List.nth branch_asts 0));((APPL (intern_false,[])),(List.nth branch_asts 1))]
        else bf branch_asts 1*)


exception BadReify of exp

let get_the_type t =
    let (evm, env) = Lemmas.get_current_context() in
        EConstr.Unsafe.to_constr (Typing.unsafe_type_of env (Evd.empty) (EConstr.of_constr t))

let rec build_term e tenv env = match e with
  | (NUM x) -> Lib_coq.Nat.of_int x
  | (APPL (4,[])) -> (Lazy.force reify_true_val)
  | (APPL (5,[])) -> (Lazy.force reify_false_val)
  | (APPL (9,l)) -> build_and_term l tenv env
  | (APPL (10,l)) -> build_or_term l tenv env
  | (APPL (11,[a;b])) -> let t1=build_term a tenv env in
                         let t2=build_term b tenv env in
                         let t = try get_the_type t1 with NoTypeInfo -> get_the_type t2 in
                  debug_print "build_term" "equal_build";Term.mkApp (Lazy.force reify_eq_val, [| t;t1;t2 |])
  | (APPL (17,[a])) -> Term.mkApp (Lazy.force reify_not_val, [| build_term a tenv env |])
  (*| (APPL (21,[])) -> (Lazy.force reify_nil)*)
  (*| (APPL (22,[f;e])) -> Term.mkApp(Lazy.force reify_cons,[|build_term f tenv env;build_term e tenv env|])*)
  | (APPL (75,[f;e])) -> Term.mkApp(build_term f tenv env,[| build_term e tenv env|])
  | (APPL (76,l)) -> build_add_term l tenv env
  | (APPL (77,[l;r])) -> Term.mkApp((Lazy.force reify_sub),[|build_term l tenv env;build_term r tenv env|])
  | (APPL (78,l)) -> build_mul_term l tenv env
  | (APPL (79,[l;r])) -> Term.mkApp(Lazy.force reify_div,[|build_term l tenv env;build_term r tenv env|])
  | (APPL (80,[l;r])) -> Term.mkApp(Lazy.force reify_lt_val,[|build_term l tenv env;build_term r tenv env|])
  | (APPL (90,[l;r])) -> Term.mkApp(Lazy.force reify_imply_val,[|build_term l tenv env;build_term r tenv env|])
  | (APPL (f,l)) -> if Renv.isAC env f || Renv.isA env f then
                        let ff = functor_from_name f in
                            build_ac_term ff l tenv env
                    else
                        let _ = debug_print "build_term" "Here0" in
                        let _ = debug_print "build_term" (decode f) in
                        let _ = debug_print "build_term" (string_of_int (List.length l)) in
                        let x = List.map (fun x -> build_term x tenv env) l in
                        let ff = functor_from_name f in
                        let _ = debug_print "build_term" "Here0.5" in
                        let tt = get_the_type ff in
                        let _ = debug_print "build_term" "Here0.7" in
                        let _ = debug_print_ast "build_name" ff in
                        let _ = debug_print "build_term" "Here1" in
                        let _ = List.map (fun x -> debug_print "build_term" (prExp x)) l in
                        let _ = debug_print "build_term" "Here2" in
                        let _ = List.map (fun x -> debug_print_ast "build_term" x) x in
                        let _ = debug_print "build_term" "Here3" in
                        let _ = debug_print "build_term" ((decode f)^" "^(string_of_int (List.length x))) in
                        let r = Term.mkApp(ff,Array.of_list x) in
                        let _ = debug_print "build_term" "Here5" in
                        let _ = debug_print_ast "build_term" r in
                        let _ = debug_print "build_term" "Done build term" in r
  | (VAR x) -> build_var (decode x) tenv
  | (QUANT (73,[(v,t)],e,p)) -> 
    let tenv' = push_var (decode v) t tenv in
        Term.mkLambda (Name (Id.of_string (decode v)),(build_coq_type t),build_term e tenv' env)
  | (CASE (e,t,c)) -> buildMatch e t c tenv env
  | x -> debug_print "build_term" (" No  match  for " ^ (prExp x) ^ "\n"); raise (BadReify(x))
  (*| _ -> (Lazy.force reify_false_val)*)
and build_ac_term ff l tenv env = match l with
  | [a] -> build_term a tenv env
  | (f::r) -> debug_print "build_ac_term" "Here a";Term.mkApp(ff,[|build_term f tenv env;build_ac_term ff r tenv env|])
and build_mul_term l tenv env = match l with
  | [] -> Lib_coq.Nat.of_int 1
  | [a] -> build_term a tenv env
  | (f::r) -> debug_print "build_mul_term" "Here a";Term.mkApp(Lazy.force reify_mul,[|build_term f tenv env;build_mul_term r tenv env|])
and build_add_term l tenv env = match l with
  | [] -> Lib_coq.Nat.of_int 0
  | [a] -> build_term a tenv env
  | (f::r) -> debug_print "build_add_term" "Here b";Term.mkApp(Lazy.force reify_add,[|build_term f tenv env;build_add_term r tenv env|])
and build_and_term l tenv env = match l with
  | [] -> Lazy.force reify_true_val
  | [a] -> build_term a tenv env
  | (f::r) -> debug_print "build_and_term" "here c";Term.mkApp(Lazy.force reify_and_val,[|build_term f tenv env;build_and_term r tenv env|])
and build_or_term l tenv env = match l with
  | [] -> Lazy.force reify_false_val
  | [a] -> build_term a tenv env
  | (f::r) -> debug_print "build_or_term" "here d";Term.mkApp(Lazy.force reify_or_val,[|build_term f tenv env;build_or_term r tenv env|])
and buildMatch e (t : Rtype.etype) cases tenv env =
    let constructors = constructorList t in
    let terms = List.map (fun (c,l) -> build_term (buildCase t c cases) tenv env) constructors in
    let eterm = build_term e tenv env in
    let ci = List.hd (!case_infos) in
    let _ = (case_infos := if List.length (!case_infos) > 0 then List.tl (!case_infos) else (!case_infos)) in
    let tterm = mkLambda (Name (Id.of_string "x"),(build_coq_type t),(get_case_type cases tenv)) in
    (*let ci = make_case_info Global.env ind RegularStyle in*)
    mkCase (ci, tterm, eterm, Array.of_list terms)
  ;;

exception BadBuild;;

let rec build_predicate e tenv env = match e with
  | (APPL (4,[])) -> (Lazy.force reify_true)
  | (APPL (5,[])) -> (Lazy.force reify_false)
  | (APPL (9,l)) -> build_and l tenv env
  | (APPL (10,l)) -> build_or l tenv env
  | (APPL (11,[a;b])) -> let t1=build_term a tenv env in
                         let t2=build_term b tenv env in
                         let t = try get_the_type t1 with NoTypeInfo -> get_the_type t2 in
                  debug_print "build_predicate" "equal_build";Term.mkApp (Lazy.force reify_eq, [| t;t1;t2 |])
  | (APPL (17,[x])) -> Term.mkProd(Anonymous,build_predicate x tenv env,(Lazy.force reify_false))
  | (APPL (80,[a;b])) -> debug_print "build_predicate" "here f";Term.mkApp (Lazy.force reify_lt, [| build_term a tenv env;build_term b tenv env|])
  | (APPL (75,[f;e])) -> let _ = debug_print "build_predicate" "building0" in
                         let _ = debug_print "build_predicate" (prExp e) in
                         let te = build_term e tenv env in
                         let _ = debug_print "build_predicate" "Test" in
                         (*let t = get_the_type te in*)
                         let _ = debug_print "build_predicate" ("building1 "^(prExp f)^" "^(prExp(APPL (75,[f;e])))) in
                         let r = Term.mkApp(build_term f tenv env,[|te|]) in
                         let _ = debug_print_ast "build_predicate" r in
                         let _ = debug_print "build_predicate" "Done building" in r
  | (APPL (90,[l;r])) -> Term.mkProd(Anonymous,build_predicate l tenv env,build_predicate r tenv env)
  | (QUANT (14,vtl,e,p)) -> push_uvars tenv vtl e env
  | (QUANT (15,vtl,e,p)) -> push_evars tenv vtl e env
  | (QUANT (73,[(v,t)],e,p)) -> 
    let tenv' = push_var (decode v) t tenv in
        Term.mkLambda (Name (Id.of_string (decode v)),(build_coq_type t),build_predicate e tenv' env)
  | (APPL (f,l)) -> let _ = debug_print "build_predicate" ("Functor " ^ (decode f)) in
                    let x = List.map (fun x -> build_term x tenv env) l in
                    let ff = functor_from_name f in
                    let _ = debug_print "build_predicate" "here g" in
                    let r = Term.mkApp(ff,Array.of_list x) in
                        if is_bool_type r tenv then
                            let coq_init_specif = ModPath.MPfile (DirPath.make (List.map Id.of_string ["Datatypes"; "Init"; "Coq"])) in
                            let sigT = mkInd (MutInd.make1 (KerName.make2 coq_init_specif (Label.make "bool")), 0) in
                            let true_term = mkConstruct (fst (destInd sigT), 1) in
                            (debug_print "build_predicate" "bool equality";
                            Term.mkApp(Lazy.force reify_eq,[|(Lazy.force reify_bool);true_term;r|]))
                        else (debug_print "build_predicate" "no bool equality";r)
  | _ -> debug_print "build_predicate" ("buildPredicate " ^ (prExp e));raise BadBuild
and push_uvars tenv vtl e env = match vtl with
  | ((v,t)::r) -> let tenv' = push_var (decode v) t tenv in
                  let _ = debug_print "push_uvars" ("Building uvar type for "^(decode v)) in
                  let ct = build_coq_type t in
                  let _ = debug_print "push_uvars" "Done coq type (now push uvars)" in
                  let pr = push_uvars tenv' r e env in
                  let _ = debug_print_ast "push_uvars" ct in
                  let _ = debug_print_ast "push_uvars" pr in
                  let _ = debug_print "push_uvars" ("Done push_uvars "^(Rtype.unparse t)) in
                  let r = Term.mkProd ((Name (Id.of_string (decode v))),ct,pr) in
                  let _ = debug_print "push_uvars" "Done making r" in r
  | _ -> build_predicate e tenv env
and push_evars tenv vtl e env = match vtl with
  | ((v,t)::r) -> let tenv' = push_var (decode v) t tenv in
                  let _ = debug_print "build_predicate" "build_evars" in
                  let typ = build_coq_type t in
                  let _ = debug_print "build_predicate" ("build_evars1 " ^ (prExp e)) in
                  let rr = (push_evars tenv' r e env) in
                  let _ = debug_print "build_predicate" "build_evars2" in
                  let lam = Term.mkLambda ((Name (Id.of_string (decode v))),typ,rr) in
                  let _ = debug_print "build_predicate" "build_evars3" in
                  let r = Term.mkApp (Lazy.force reify_ex, [|typ;lam|]) in
                  let _ = debug_print "build_predicate" "end build_evars" in r
  | _ -> debug_print "build_predicate" "Done evars";build_predicate e tenv env
and build_and l tenv env = match l with
  | [] -> Lazy.force reify_true
  | [a] -> build_predicate a tenv env
  | (f::r) -> debug_print "build_and" "here h";Term.mkApp(Lazy.force reify_and,[|build_predicate f tenv env;build_and r tenv env|])
and build_or l tenv env = match l with
  | [] -> Lazy.force reify_false
  | [a] -> build_predicate a tenv env
  | (f::r) -> debug_print "build_or" "here i";Term.mkApp(Lazy.force reify_or,[|build_predicate f tenv env;build_or r tenv env|])
  ;;


let rec print_type_class (c : Typeclasses.instance) =
    let impl = Typeclasses.instance_impl c in
    let c = printable_constr_of_global impl in
    let n = match kind_of_term c with
            | Const (c, u) ->
                  let kn = Constant.canonical c in
                  let kn' = build_kername kn in kn'
            | _ -> "" in
    (*let ast = apply_to_definition build_ast (Global.env ()) 1 c in*)
        debug_print "typeclasses" n;
        if String.length n > 3 && String.sub n 0 3 ="tri" then
            let se = match kind_of_term c with
                    | Const (c, u) ->
                      let kn = Constant.canonical c in
                      let cd = (Environ.lookup_constant c (Global.env ())) in
                          match get_definition cd with
                          | None -> NOEXP
                          | Some c ->
                          build_definition_exp kn (build_exp (Global.env ()) c) u
                    | _ -> NOEXP in
                debug_print "typeclasses" (prExp se) ;
                debug_print_ast "typeclasses" c
        else ()
  ;;

let rec print_type_classes cl =
  match cl with
  | (f::r) -> ((print_type_class f);(print_type_classes r))
  | _ -> ()
  ;;

let typeclass_ac = intern "C_AdvancedRewrite.advancedRewrite.AC_PROP 1" ;;
let typeclass_a = intern "C_AdvancedRewrite.advancedRewrite.A_PROP 1" ;;
let typeclass_c = intern "C_AdvancedRewrite.advancedRewrite.C_PROP 1" ;;
let typeclass_eq = intern "C_AdvancedRewrite.advancedRewrite.EQ_PROP 1" ;;
let typeclass_po = intern "C_AdvancedRewrite.advancedRewrite.PO_PROP 1" ;;
let typeclass_to = intern "C_AdvancedRewrite.advancedRewrite.TO_PROP 1" ;;
let typeclass_epo = intern "C_AdvancedRewrite.advancedRewrite.EPO_PROP 1" ;;
let typeclass_eto = intern "C_AdvancedRewrite.advancedRewrite.ETO_PROP 1" ;;
let typeclass_eqp = intern "C_AdvancedRewrite.advancedRewrite.EQP_PROP 1" ;;
let typeclass_pop = intern "C_AdvancedRewrite.advancedRewrite.POP_PROP 1" ;;
let typeclass_top = intern "C_AdvancedRewrite.advancedRewrite.TOP_PROP 1" ;;
let typeclass_epop = intern "C_AdvancedRewrite.advancedRewrite.EPOP_PROP 1" ;;
let typeclass_etop = intern "C_AdvancedRewrite.advancedRewrite.ETOP_PROP 1" ;;
let typeclass_rewrite_rule = intern "C_AdvancedRewrite.advancedRewrite.REWRITE_RULE_PROP 1" ;;
let typeclass_prec_less = intern "C_AdvancedRewrite.advancedRewrite.PREC_LESS_PROP 1" ;;
let typeclass_prec_equal = intern "C_AdvancedRewrite.advancedRewrite.PREC_EQUAL_PROP 1" ;;

let rec add_type_class_decl env se =
  (debug_print "typeclasses" "Processing";
   debug_print "typeclasses" (prExp se));
  match se with
  | (APPL (d,[(APPL (_,[(APPL (ac,[_;(APPL (f,[]));q]))]))])) ->
    debug_print "typeclasses" "pattern1";
    if ac=typeclass_ac then
        ((debug_print "typeclasses" ("AC " ^ (decode f)));(Renv.addAttrib env intern_ac [Renv.S(f)]))
    else if ac=typeclass_a then
        ((debug_print "typeclasses" ("A " ^ (decode f)));(Renv.addAttrib env intern_a [Renv.S(f)]))
    else if ac=typeclass_c then
        ((debug_print "typeclasses" ("C " ^ (decode f)));(Renv.addAttrib env intern_c [Renv.S(f)]))
    else if ac=typeclass_eq then
        ((debug_print "typeclasses" ("EQ " ^ (decode f)));(Renv.addAttrib env intern_eq [Renv.S(f)]))
    else if ac=typeclass_eqp then
        ((debug_print "typeclasses" ("EQP " ^ (decode f)));(Renv.addAttrib env intern_eq [Renv.S(f)]))
    else
        env
  | (APPL (d,[(APPL (_,[(APPL (ac,[_;(APPL (f,[]));(APPL (g,[]));_]))]))])) ->
    debug_print "typeclasses" "pattern2";
    if ac=typeclass_po then
        ((debug_print "typeclasses" ("PO " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_po [Renv.S(g);Renv.S(f)]))
    else if ac=typeclass_to then
        ((debug_print "typeclasses" ("TO " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_to [Renv.S(g);Renv.S(f)]))
    else if ac=typeclass_epo then
        ((debug_print "typeclasses" ("EPO " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_epo [Renv.S(g);Renv.S(f)]))
    else if ac=typeclass_eto then
        ((debug_print "typeclasses" ("ETO " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_eto [Renv.S(g);Renv.S(f)]))
    else if ac=typeclass_pop then
        ((debug_print "typeclasses" ("POP " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_po [Renv.S(g);Renv.S(f)]))
    else if ac=typeclass_top then
        ((debug_print "typeclasses" ("TOP " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_to [Renv.S(g);Renv.S(f)]))
    else if ac=typeclass_epop then
        ((debug_print "typeclasses" ("EPOP " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_epo [Renv.S(g);Renv.S(f)]))
    else if ac=typeclass_etop then
        ((debug_print "typeclasses" ("ETOP " ^ (decode f) ^ " " ^ (decode g)));(Renv.addAttrib env intern_eto [Renv.S(g);Renv.S(f)]))
    else
        env
  | (APPL (d,[(APPL (_,[(APPL (ac,[_;_;(APPL (f,[]));(APPL (g,[]))]))]))])) ->
    debug_print "typeclasses" "pattern3";
    if ac=typeclass_prec_less then
        (debug_print "typeclasses" ("LESS PREC " ^ (decode f) ^ " " ^ (decode g));
        Renv.addPrecedence env (f,g))
    else if ac=typeclass_prec_equal then
        (debug_print "typeclasses" ("EQUAL PREC " ^ (decode f) ^ " " ^ (decode g));
        Renv.addEqualPrecedence env (f,g))
    else env
  (*| (APPL (d,[(APPL (_,[(APPL (ac,[p;_]))]))])) ->
    debug_print "typeclasses" "Rewrite Rule pattern";
    if ac=typeclass_rewrite_rule then
        ((debug_print "typeclasses" ("REWRITE_RULE " ^ (prExp p)));(process_property env p))
    else
        env*)
  | q -> debug_print "typeclasses" "funny def";env

let rec add_type_class_rewrite_decl env se =
  (debug_print "typeclasses" "Processing";
   debug_print "typeclasses" (prExp se));
  match se with
  | (APPL (d,[(APPL (_,[(APPL (ac,[p;_]))]))])) ->
    debug_print "typeclasses" "Rewrite Rule pattern";
    if ac=typeclass_rewrite_rule then
        ((debug_print "typeclasses" ("REWRITE_RULE " ^ (prExp p)));(process_property env p))
    else
        env
  | q -> debug_print "typeclasses" "funny def2";env

let rec build_type_class_env env (c : Typeclasses.instance) =
    let _ = debug_print "typeclasses" "build_type_class_env" in
    let impl = Typeclasses.instance_impl c in
    let c = printable_constr_of_global impl in
    let n = match kind_of_term c with
            | Const (c, u) ->
                  let kn = Constant.canonical c in
                  let kn' = build_kername kn in kn'
            | _ -> "" in
        debug_print "typeclasses" n;
        if String.length n > 3 && not(String.sub n 0 3="Coq") then
            let se = match kind_of_term c with
                    | Const (c, u) ->
                      let kn = Constant.canonical c in
                      let cd = (Environ.lookup_constant c (Global.env ())) in
                          match get_definition cd with
                          | None -> NOEXP
                          | Some c ->
                          build_definition_exp kn (build_exp (Global.env ()) c) u
                    | _ -> NOEXP in
             add_type_class_decl env se
        else env

let rec build_typeclasses_env env cl =
  match cl with
  | (f::r) -> build_typeclasses_env (build_type_class_env env f) r
  | _ -> env
  ;;

let rec build_type_class_rewrite_env env (c : Typeclasses.instance) =
    let _ = debug_print "typeclasses" "build_type_class_env" in
    let impl = Typeclasses.instance_impl c in
    let c = printable_constr_of_global impl in
    let n = match kind_of_term c with
            | Const (c, u) ->
                  let kn = Constant.canonical c in
                  let kn' = build_kername kn in kn'
            | _ -> "" in
        debug_print "typeclasses" n;
        if String.length n > 3 && not(String.sub n 0 3="Coq") then
            let se = match kind_of_term c with
                    | Const (c, u) ->
                      let kn = Constant.canonical c in
                      let cd = (Environ.lookup_constant c (Global.env ())) in
                          match get_definition cd with
                          | None -> NOEXP
                          | Some c ->
                          build_definition_exp kn (build_exp (Global.env ()) c) u
                    | _ -> NOEXP in
             add_type_class_rewrite_decl env se
        else env

let rec build_typeclasses_rewrite_env env cl =
  match cl with
  | (f::r) -> build_typeclasses_rewrite_env (build_type_class_rewrite_env env f) r
  | _ -> env
  ;;

(* Top-level arewrite functionality *)
let arewrite cl : unit Proofview.tactic =
  Proofview.Goal.enter (fun gl ->
  let _ = debug_print "arewrite" "CLEARING CACHE" in
  let _ = (constr_cache := StringMap.empty) in
  let _ = (def_cache := StringMap.empty) in
  let _ = debug_print "typeclasses" "Test\n" in
  let _ = print_type_classes (Typeclasses.all_instances ()) in
  let _ = debug_print "arewrite" ("Environment:\n\n" ^ string_of_ppcmds (print_full_pure_context ()) ^ "\nEND\n\n") in
  let rewriteEnv1 = (*build_rewrite_env*) Renv.emptyEnv in
  let rewriteEnv2 = build_typeclasses_env rewriteEnv1 (Typeclasses.all_instances ()) in
  let rewriteEnv = build_typeclasses_rewrite_env rewriteEnv2 (Typeclasses.all_instances ()) in
  let concl = Proofview.Goal.concl gl in
  let (evm, env) = Lemmas.get_current_context() in
  (*let (body, _) = Constrintern.interp_constr env evm concl in*)
  let _ = debug_print "arewrite" "******* BEGIN *******" in
  let _ = debug_print_ast "arewrite" (EConstr.Unsafe.to_constr concl) in
  let _ = debug_print "arewrite" "******* END *******" in
  let e = build_exp env (EConstr.Unsafe.to_constr concl) in
  let _ = debug_print "arewrite" "Rewriting " in
  let _ = debug_print "arewrite" (prExp e) in
  let names = Context.Named.fold_outside (fun d l -> (Context.Named.Declaration.get_id d)::l) (Proofview.Goal.hyps gl) ~init:[] in
  let operands = Locusops.concrete_clause_of (fun () -> names) cl in
  let _ = List.map (fun x -> match x with
                | Locus.OnConcl _ -> debug_print "arewrite" "Simplifying CONCLUSION"
                | Locus.OnHyp (id,_,_) -> debug_print "arewrite" ("Simplifying hyp "^(Names.Id.to_string id))
              ) operands in
  let (renv,tl) = Context.Named.fold_outside (fun d (re,l) -> (debug_print "arewrite" ("HYP: " ^ (Names.Id.to_string (Context.Named.Declaration.get_id d))));
             let t = Context.Named.Declaration.get_type d in
             let i = Context.Named.Declaration.get_id d in
             let (oc,hf) = Locusops.occurrences_of_hyp i cl in
             let ot = match oc with
                      | Locus.NoOccurrences -> "No occurences"
                      | Locus.AllOccurrences -> "All occurences"
                      | Locus.AllOccurrencesBut _ -> "All occurences but"
                      | Locus.OnlyOccurrences _ -> "Only occurences" in
             let _ = debug_print "arewrite" ("hn " ^ (Names.Id.to_string i) ^ " " ^ ot) in
             let ee = build_exp env (EConstr.Unsafe.to_constr t) in
             let re2 = Crewrite.add_rule re e (APPL (intern_oriented_rule,[ee;(APPL (intern_true,[]));(APPL (intern_true,[]))])) in
             let l2 = if oc=Locus.NoOccurrences then l else
                 let ee2 = List.hd (Inner.rewrite2 rewriteEnv (Renv.flatten rewriteEnv ee)) in
                 let _ = debug_print "arewrite" ("ee2 = " ^ (prExp ee2)) in
                     (Equality.replace_in_clause_maybe_by t (EConstr.of_constr (build_predicate ee2 [] rewriteEnv)) cl None)::l in
             let _ = debug_print "arewrite" ("\n\n" ^ (prExp ee) ^ "\n") in
                 (re2,l2)) (Proofview.Goal.hyps gl) ~init:(rewriteEnv,[]) in
             let tl2 = if Locusops.occurrences_of_goal cl=Locus.NoOccurrences then tl else 
               let e' = List.hd (Inner.rewrite2 renv (Renv.flatten renv e)) in
               let _ = debug_print "arewrite" "Result\n" in
               let _ = debug_print "arewrite" (prExp e') in
               let pr = build_predicate e' [] renv in
               let _ = debug_print "arewrite" "Built predicate\n" in
               let _ = debug_print_ast "arewrite" pr in
               let _ = debug_print "arewrite" "End\n" in
	       (Equality.replace_in_clause_maybe_by concl (EConstr.of_constr pr) cl None)::tl in
  (
   (*Tacticals.New.tclFAIL 1
    (Pp.str "The tactic is not imlplemented.")*)
          Tacticals.New.tclTHENLIST tl2
            (*[
              (** Our list of tactics consists in the following single
                  tactic, that changes the conclusion of the goal to
                  [concl'] if [concl] and [concl'] are convertible.
                  (see [tactics/tactis.mli] for other tactics.)  *)
              Equality.replace_in_clause_maybe_by concl (EConstr.of_constr (build_predicate e' [])) cl None;
            ]*)))
end

(** We reify the structure of coq expressions as an ocaml
    data-type. We reify only the structure of the expression
    w.r.t. the [plus], [S], and [O] symbols of Coq. All other
    sub-expressions are stored in an environment.
*)
module Arith = struct

  (** First, we initialise some constants from Coq standard library.*)
  let plus = lazy (Lib_coq.init_constant ["Coq"; "Init"; "Peano"] "plus")
  let succ = lazy (Lib_coq.init_constant ["Coq"; "Init"; "Datatypes"] "S")
  let zero = lazy (Lib_coq.init_constant ["Coq"; "Init"; "Datatypes"] "O")

  (** [t] is an algebraic data-type that represents reified arithemtic
      expressions *)
  type t =
    | Plus of (t * t)
    | Const of int 		      
    | Succ of t 
    | Var of int 		       

	
  let quote (env : Lib_coq.Env.t) (c : Term.constr) : t =
    (** First, we force the constants, once and for all  *)
    let plus = Lazy.force plus in 
    let succ = Lazy.force succ in 
    let zero = Lazy.force zero in 
    (** Second, we decompose recursively the given term.  If the term
	is an application, we compare the head-symbol with [plus] and
	[succ]. If the term is equal to [zero], we build a
	constant. In any other case, we have to add a new variable to
	the reification environement. *)
    let rec aux c = match Lib_coq.decomp_term c with
      | Term.App (head,args) 
	  when Term.eq_constr head plus && Array.length args = 2
	  -> Plus (aux args.(0), aux args.(1))
      | Term.App (head,args) 
	  when Term.eq_constr head succ && Array.length args =  1 
	  ->
	(** a small match to get a intelligible representation of
	    constants. *)
	begin match (aux args.(0)) with 
	  | Const i -> Const (i +1)
	  | e -> Succ e
	end
      | _ when Term.eq_constr c zero ->
	Const 0
      | _ ->
	let i = Lib_coq.Env.add env c in
	Var i
    in
    aux c
end

(** Now that we have reified the structure of the term inside ocaml,
    we will reify it inside Coq (this is also the purpose of the Quote
    module of standard Coq). 
*)
module Reif = struct
  (** We initialize a new bunch of constants that correspond to the
      constructors of our inductive. *)
    
  (** This [path] correspond to the name of the logical directory
      (ML_tutorial), and the name of the library (Theory). The name of
      the logical directory must be consistent with the options given
      to coq_makefile: [-R ./src ML_tutorial] adds the physical
      directory [src] as the logical directory [ML_tutorial].
  *)
  let path = ["ML_tutorial";"Theory"] 

  let plus = lazy (Lib_coq.init_constant  path "a_plus")
  let var = lazy (Lib_coq.init_constant  path "a_var")
  let const = lazy (Lib_coq.init_constant path  "a_const")
  let succ = lazy (Lib_coq.init_constant path "a_succ")

  (** [eval] is the Coq function that maps a reified Coq arithmetic
      expression back to a nat *)
  let eval = lazy(Lib_coq.init_constant path "eval")

  (** [to_constr t] build the Coq term that corresponds to [t]. *)
  let rec to_constr (t : Arith.t) : Term.constr =  match t with
      | Arith.Plus (a, b) -> Term.mkApp (Lazy.force plus, [|(to_constr a); (to_constr b)|])
      | Arith.Const n -> Term.mkApp (Lazy.force const, [|Lib_coq.Nat.of_int n|])
      | Arith.Succ a -> Term.mkApp (Lazy.force succ, [|(to_constr a)|])
      | Arith.Var n ->  Term.mkApp (Lazy.force var, [|Lib_coq.Nat.of_int n|])
	
  (** [env_to_constr env] build the Coq list that correspond to the
      environment map. We build a uniform Coq list of nat of type
      [list nat]. More complex situations may be treated in subsequent
      tutorials. *)
  let env_to_constr (env : Lib_coq.Env.t) : Term.constr = 
    let l = Lib_coq.Env.to_list env in 
    Lib_coq.List.of_list (Lazy.force Lib_coq.Nat.typ) l
      
  (** [build_eval env t] builds the Coq term that corresponds to [eval
      env t]. *)
  let build_eval (env : Term.constr) (t : Arith.t) : Term.constr =
    Lib_coq.lapp eval [|env; to_constr t|]
  (* alternatively, 
     Term.mkApp (Lazy.force eval, [|env_to_constr env; to_constr t|]) *)
      
  (** [tac] is the final tactic. *)
  let tac : unit Proofview.tactic =
      Proofview.Goal.enter (fun gl ->
      (** We get the conclusion of the as a goal, which is a constr.
          (see [proofs/proofview.mli].)  *)
      let concl = Proofview.Goal.concl gl in
      
      (** In our particular setting, the conclusion of the goal must
	  be a relation applied to at least two arguments (the
	  left-hand side and the right-hand side) of the
	  "equation".  *)
      match Lib_coq.decomp_term (EConstr.Unsafe.to_constr concl) with
	| Term.App(c, args) when Array.length args >= 2 ->
          let n = Array.length args in
       	  let left = args.(n-2) in
       	  let right = args.(n-1) in 
	  (** We initialize the environment, to reify the left
	      hand-side and the right-hand side of the equation*)
       	  let arith_env = Lib_coq.Env.empty () in
       	  let left' = Arith.quote arith_env left in
       	  let right' = Arith.quote arith_env right in
	  let coq_env = env_to_constr arith_env in
	  (** We want to move from 
	      {C left == right}
	      to
	      {C (eval env left') == (eval env right')}
	      
	  *)
          args.(n-2) <- build_eval coq_env left';
          args.(n-1) <- build_eval coq_env right';
       	  let concl' = Term.mkApp (c, args)
	  in
	  (** We use a {i tactical} to chain together a list of
	      tactics (as would be done using a semi-column in Coq).
	      (see [tactics/tacticals.mli] for other tacticals.)  *)
       	  Tacticals.New.tclTHENLIST
	    [
	      (** Our list of tactics consists in the following single
       		  tactic, that changes the conclusion of the goal to
       		  [concl'] if [concl] and [concl'] are convertible. 
		  (see [tactics/tactis.mli] for other tactics.)  *)
	      Tactics.change_concl (EConstr.of_constr concl') ;
	    ]
	| _ -> 
	  (** If the goal was not looking like a relation applied to two
	      arguments, we fail using the tacticals [tclFAIL]. 
	      
	      The documentation of fail is
	      {{:http://coq.inria.fr/refman/Reference-Manual012.html#@tactic183}here}

	      In a nutshell [tclFAIl] has type [int -> Pp.std_ppcmds ->
	      tactic]. The number is the failure level (0 means that
	      an englobing [match goal] may proceed to the next clause
	      and [try] succeeds, while n > 1 means that the current
	      [match goal] or [try] is aborted, and the level is
	      decremented. 

	      The [Pp.std_ppcmds] is a pretty-printer command. 
	      
	      (see lib/pp.mli for more functions)
	  *)
	  Tacticals.New.tclFAIL 1
	    (Pp.str "The goal does not look like an equation"))
end
  
(** The final magic part is to register our custom tactic in
    Coq. [_reflect_] is the name of this tactic extension (I do not know
    what it is used for). [Reif.tac] is our custom
    tactic. [reflect_arith] is the string through which this tactic
    can be invoked inside Coq. 
*)

open Pcoq
open Pcoq.Constr
open Pltac

TACTIC EXTEND AR2
| ["arewrite" clause(cl)] -> [Stuff.arewrite cl]
END

(* PrintAST command
   The depth specifies the depth at which to unroll nested type definitions *)
VERNAC COMMAND EXTEND AR
| [ "printAST" constr(def) ] -> [ Stuff.print_ast 0 def ]
| [ "printExp" constr(def) ] -> [ Stuff.print_exp 0 def ]
| [ "printAST" constr(def) "with" "depth" integer(depth)] -> [ Stuff.print_ast depth def ]
END

(*Rtrace.toggle_trace () ;;*)

