(******************************************************************************
 *
 * REWRITELIB
 *
 * advanced_rewrite.ml4
 *
 * This file contains the interface between Coq and the rewriting library
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

DECLARE PLUGIN "advanced_rewrite"

open Declarations
open Format
open Univ
open Term
open Names

open Intern
open Exp

(*
 * Plugin to print an s-expression representing the (possibly expanded) AST for a definition.
 * Based on TemplateCoq: https://github.com/gmalecha/template-coq/blob/master/src/reify.ml4
 *
 * Consider this plugin a learning tool to help understand how to traverse the AST, which is why it is extensively commented.
 * It's also why even simple functions are highly separated out instead of nested, which is not typical OCaml style.
 * Feel free to fork it and mess around with the functions to see what happens.
 *)

(* --- Options --- *)

(*
 * Toggles between DeBruijn indexing and names
 *)
let opt_debruijn = ref (false)
let _ = Goptions.declare_bool_option {
  Goptions.optsync = true;
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
  Goptions.optsync = true;
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
let print (s : string) =
  Pp.pp (Pp.str s)

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
  let (name, body, typ) = Environ.lookup_rel i env in
  build_name name

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
    let (name, body, typ) = Environ.lookup_rel i env in
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
  match e with
  | (APPL (f,l)) -> Rtype.mkProduct (map_type f) (List.map convert_exp_to_type l)
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
 | OpaqueDef o -> (* https://coq.inria.fr/refman/Reference-Manual008.html#Opaque*)
     Some (Opaqueproof.force_proof (Global.opaque_tables ()) o)

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
      (fun i name typ -> (name, None, Vars.lift i typ))
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
  APPL ((intern "Fix"),[APPL ((intern "Functions"),funs); NUM index])

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
  Environ.lookup_mind i env

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
  List.map
    (fun ind_body ->
      let univ_context = mutind_body.mind_universes in
      let univ_instance = UContext.instance univ_context in
      let name_id = ind_body.mind_typename in
      let mutind_spec = (mutind_body, ind_body) in
      let typ = Inductive.type_of_inductive env (mutind_spec, univ_instance) in
      (Names.Name name_id, None, typ))
    ind_bodies

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
        else bf branch_asts 1

let build_case_exp (info : case_info) (case_typ_ast : exp) (match_ast : exp) (branch_asts : exp list) =
  let num_args = info.ci_npar in
  let match_typ = APPL ((intern "CaseMatch"),[match_ast]) in
  let (QUANT (l,[(v,t)],_,_)) = case_typ_ast in
  (*let _ = print_string ((Rtype.pretype t)^"\n") in*)
  let branches = APPL ((intern "CaseBranches"),branch_asts) in
  CASE (match_ast,convert_exp_to_type case_typ_ast,(getConstructors t branch_asts))
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

(* --- Full AST --- *)

let rec build_ast (env : Environ.env) (depth : int) (trm : types) =
  match kind_of_term trm with
    Rel i ->
      build_rel env i
  | Var v ->
      build_var v
  | Meta mv ->
      build_meta mv
  | Evar (k, cs) ->
      let cs' = List.map (build_ast env depth) (Array.to_list cs) in
      build_evar k cs'
  | Sort s ->
      build_sort s
  | Cast (c, k, t) ->
      let c' = build_ast env depth c in
      let t' = build_ast env depth t in
      build_cast c' k t'
  | Prod (n, t, b) ->
      let t' = build_ast env depth t in
      let b' = build_ast (Environ.push_rel (n, None, t) env) depth b in
      build_product n t' b'
  | Lambda (n, t, b) ->
      let t' = build_ast env depth t in
      let b' = build_ast (Environ.push_rel (n, None, t) env) depth b in
      build_lambda n t' b'
  | LetIn (n, trm, typ, b) ->
      let trm' = build_ast env depth trm in
      let typ' = build_ast env depth typ in
      let b' = build_ast (Environ.push_rel (n, Some b, typ) env) depth b in
      build_let_in n trm' typ' b'
  | App (f, xs) ->
      let f' = build_ast env depth f in
      let xs' = List.map (build_ast env depth) (Array.to_list xs) in
      build_app f' xs'
  | Const (c, u) ->
      build_const env depth (c, u)
  | Construct ((i, c_index), u) ->
      let i' = build_ast env depth (Term.mkInd i) in
      build_constructor i' c_index u
  | Ind ((i, i_index), u) ->
      build_minductive env depth ((i, i_index), u)
  | Case (ci, ct, m, bs) ->
      let typ = build_ast env depth ct in
      let match_typ = build_ast env depth m in
      let branches = List.map (build_ast env depth) (Array.to_list bs) in
      build_case ci typ match_typ branches
  | Fix ((is, i), (ns, ts, ds)) ->
      build_fix (build_fixpoint_functions env depth ns ts ds) i
  | CoFix (i, (ns, ts, ds)) ->
      build_cofix (build_fixpoint_functions env depth ns ts ds) i
  | Proj (p, c) ->
      let p' = build_ast env depth (Term.mkConst (Projection.constant p)) in
      let c' = build_ast env depth c in
      build_proj p' c'

and build_const (env : Environ.env) (depth : int) ((c, u) : pconstant) =
  let kn = Constant.canonical c in
  let cd = Environ.lookup_constant c env in
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
      build_rel_exp env i
  | Var v ->
      build_var_exp v
  | Meta mv ->
      build_exp_meta mv
  | Evar (k, cs) ->
      let cs' = List.map (build_exp env) (Array.to_list cs) in
      build_evar_exp k cs'
  | Sort s ->
      build_sort_exp s
  | Cast (c, k, t) ->
      let c' = build_exp env c in
      let t' = build_exp env t in
      build_cast_exp c' k t'
  | Prod (n, t, b) ->
      let t' = build_exp env t in
      let b' = build_exp (Environ.push_rel (n, None, t) env) b in
      build_product_exp n t' b'
  | Lambda (n, t, b) ->
      let t' = build_exp env t in
      let b' = build_exp (Environ.push_rel (n, None, t) env) b in
      build_lambda_exp n t' b'
  | LetIn (n, trm, typ, b) ->
      let trm' = build_exp env trm in
      let typ' = build_exp env typ in
      let b' = build_exp (Environ.push_rel (n, Some b, typ) env) b in
      LET (trm',Rtype.notype,typ',b')
  | App (f, xs) ->
      (match natFor trm with
       | Some n -> NUM n
       | None -> (match build_app_constant_term env f xs with
                  | Some e -> e
                  | None ->
                    (match build_app_term env f xs with
                     | Some e -> e
                     | None ->
                        let f' = build_exp env f in
                        let xs' = List.map (build_exp env) (Array.to_list xs) in
                            (APPL (intern_apply,(f'::xs'))))))
  | Const (c, u) ->
      build_const_exp env (c, u)
  | Construct ((i, c_index), u) ->
      let i' = build_exp env (Term.mkInd i) in
      let (x,_) = i in
      let s = (MutInd.to_string x) in
          if s="Coq.Init.Datatypes.nat" && c_index=1 then
              NUM 0
          else if s="Coq.Init.Datatypes.bool" && c_index=1 then
              (APPL (intern_true,[]))
          else if s="Coq.Init.Datatypes.bool" && c_index=2 then
              (APPL (intern_false,[]))
          else
              (APPL ((intern ((s^(string_of_int c_index)))),[]))
              (*build_constructor_exp i' c_index u*)
  | Ind ((i, i_index), u) ->
      (match build_inductive_term env i i_index with
       | Some x -> x
       | None -> build_minductive_exp env ((i, i_index), u))
  | Case (ci, ct, m, bs) ->
      let typ = build_exp env ct in
      let match_typ = build_exp env m in
      let branches = List.map (build_exp env) (Array.to_list bs) in
      build_case_exp ci typ match_typ branches
  | Fix ((is, i), (ns, ts, ds)) ->
      build_fix_exp (build_fixpoint_functions_exp env ns ts ds) i
  | CoFix (i, (ns, ts, ds)) ->
      build_cofix_exp (build_fixpoint_functions_exp env ns ts ds) i
  | Proj (p, c) ->
      let p' = build_exp env (Term.mkConst (Projection.constant p)) in
      let c' = build_exp env c in
      build_proj_exp p' c'
and build_inductive_term (env : Environ.env) i i_index =
    Some (match ((MutInd.to_string i),i_index) with
          | ("Coq.Init.Logic.True",_) -> (APPL (intern_true,[]))
          | ("Coq.Init.Logic.False",_) -> (APPL (intern_false,[]))
          | (x,_) -> (APPL (intern x,[])))
and build_app_term (env : Environ.env) f xs =
      (match kind_of_term f with
      | Ind ((i, c_index),u) ->
             let xs' = List.map (build_exp env) (Array.to_list xs) in
                 Some (match MutInd.to_string i with
                       | "Coq.Init.Logic.or" -> (APPL (intern_or,xs'))
                       | "Coq.Init.Logic.and" -> (APPL (intern_and,xs'))
                       | "Coq.Init.Logic.ex" ->
                              (match xs' with
                              | [t1;(QUANT (intern_lambda,[(v1,t2)],b,_))] ->
                                    (QUANT (intern_exists,[(v1,t2)],b,(APPL (intern_true,[]))))
                              | _ -> (APPL ((intern "Coq.Init.Logic.ex"),xs')))
                       | "Coq.Init.Logic.eq" -> (APPL (intern_equal,[(List.nth xs' 1);(List.nth xs' 2)]))
                       | x -> (APPL ((intern x ),xs')))
      | Const (c, u) ->
            let kn = Constant.canonical c in
            let kn' = build_kername kn in
            let xs' = List.map (build_exp env) (Array.to_list xs) in
                 Some (match kn' with
                       | "Coq.Init.Nat.add" -> (APPL (intern_nat_plus,xs'))
                       | "Coq.Init.Nat.sub" -> (APPL (intern_nat_minus,xs'))
                       | "Coq.Init.Nat.mul" -> (APPL (intern_nat_times,xs'))
                       | "Coq.Init.Peano.lt" -> (APPL (intern_nat_less,xs'))
                       | "Coq.Init.Logic.eq" -> (APPL (intern_equal,xs'))
                       | x -> (APPL ((intern x),xs')))
      | _ -> None)
and build_app_constant_term (env : Environ.env) f xs =
      (match kind_of_term f with
      | Construct ((i, c_index),u) ->
             let (x,_) = i in
             let xs' = List.map (build_exp env) (Array.to_list xs) in
                 Some (APPL ((intern ((MutInd.to_string x)^(string_of_int c_index))),xs'))
      | _ -> None)
and build_const_exp (env : Environ.env) ((c, u) : pconstant) =
  let kn = Constant.canonical c in
  let cd = Environ.lookup_constant c env in
  let global_env = Global.env () in
  match get_definition cd with
    None ->
      begin
        match cd.const_type with
          RegularArity ty -> build_axiom_exp kn (build_exp global_env ty) u
        | TemplateArity _ -> assert false (* pre-8.5 universe polymorphism *)
      end
  | Some c ->
      build_definition_exp kn (build_exp global_env c) u

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

(* Top-level print AST functionality *)
let print_exp (depth : int) (def : Constrexpr.constr_expr) =
  let (evm, env) = Lemmas.get_current_context() in
  let (body, _) = Constrintern.interp_constr env evm def in
  let ast = prExp (Renv.flatten Renv.emptyEnv (build_exp env body)) in
  print ast

(* PrintAST command
   The depth specifies the depth at which to unroll nested type definitions *)
VERNAC COMMAND EXTEND Print_AST
| [ "printAST" constr(def) ] ->
  [ print_ast 0 def ]
| [ "printExp" constr(def) ] ->
  [ print_exp 0 def ]
| [ "printAST" constr(def) "with" "depth" integer(depth)] ->
  [ print_ast depth def ]
END

