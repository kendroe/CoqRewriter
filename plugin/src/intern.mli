(******************************************************************************
 *
 * REWRITELIB
 *      
 * intern.mli
 *
 * Data Structure for interning symbols
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

val intern: string -> int
val decode: int -> string
val intern_oriented_rule: int   (* 1 *)
val intern_unoriented_rule: int (* 2 *)
val intern_bool: int            (* 3 *)
val intern_true: int            (* 4 *)
val intern_false: int           (* 5 *)
val intern_unit: int            (* 6 *)
val intern_identity: int        (* 7 *)
val intern_trivial: int         (* 8 *)
val intern_and: int             (* 9 *)
val intern_or: int              (* 10 *)
val intern_equal: int           (* 11 *)
val intern_preceq: int          (* 12 *)
val intern_defined: int         (* 13 *)
val intern_all: int             (* 14 *)
val intern_exists: int          (* 15 *)
val intern_star : int           (* 16 *)
val intern_not : int            (* 17 *)
val intern_if : int             (* 18 *)
val intern_undef : int          (* 19 *)
val intern_set : int            (* 20 *)
val intern_cons : int           (* 21 *)
val intern_nil : int            (* 22 *)
val intern_attr : int           (* 23 *)
val intern_ac : int             (* 24 *)
val intern_a : int              (* 25 *)
val intern_c : int              (* 26 *)
val intern_epo : int            (* 27 *)
val intern_eq : int             (* 28 *)
val intern_derive : int         (* 29 *)
val intern_to : int             (* 30 *)
val intern_eto : int            (* 31 *)
val intern_po : int             (* 32 *)
val intern_smi : int            (* 33 *)
val intern_csmi : int           (* 34 *)
val intern_omi : int            (* 35 *)
val intern_comi : int           (* 36 *)
val intern_smd : int            (* 37 *)
val intern_csmd : int           (* 38 *)
val intern_omd : int            (* 39 *)
val intern_comd : int           (* 40 *)
val intern_mi : int             (* 41 *)
val intern_cmi : int            (* 42 *)
val intern_md : int             (* 43 *)
val intern_cmd : int            (* 44 *)
val intern_solved : int         (* 45 *)
val intern_t : int              (* 46 *)
val intern_goal : int           (* 47 *)
val intern_node : int           (* 48 *)
val intern_branch : int         (* 49 *)
val intern_subterm : int        (* 50 *)
val intern_pattern : int        (* 51 *)
val intern_down : int           (* 52 *)
val intern_top_goal : int       (* 53 *)
val intern_top_node : int       (* 54 *)
val intern_top_branch : int     (* 55 *)
val intern_top_subterm : int    (* 56 *)
val intern_top_pattern : int    (* 57 *)
val intern_top_down : int       (* 58 *)
val intern_subgoals : int       (* 59 *)
val intern_decomp : int         (* 60 *)
val intern_less : int           (* 61 *)
val intern_plus : int           (* 62 *)
val intern_slash : int          (* 63 *)
val intern_percent : int        (* 64 *)
val intern_minus : int          (* 65 *)
val intern_size : int           (* 66 *)
val intern_char : int           (* 67 *)
val intern_concat : int         (* 68 *)
val intern_chr : int            (* 69 *)
val intern_asc : int            (* 70 *)
val intern_def : int            (* 71 *)
val intern_default : int        (* 72 *)
val intern_lambda : int         (* 73 *)
val intern_fn : int             (* 74 *)
val intern_apply : int          (* 75 *)
val intern_nat_plus : int       (* 76 *)
val intern_nat_minus : int      (* 77 *)
val intern_nat_times : int      (* 78 *)
val intern_nat_divide : int     (* 79 *)
val intern_nat_less : int       (* 80 *)
val intern_nat_mod : int        (* 81 *)
val intern_rat_plus : int       (* 82 *)
val intern_rat_minus : int      (* 83 *)
val intern_rat_times : int      (* 84 *)
val intern_rat_divide : int     (* 85 *)
val intern_rat_less : int       (* 86 *)
val intern_rat_mod : int        (* 87 *)
val intern_rat_to_nat : int     (* 88 *)
val intern_nat_to_rat : int     (* 89 *)
val intern_implies : int        (* 90 *)
val intern_fix : int            (* 91 *)

val count : unit -> int





