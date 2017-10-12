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

(* require "trie.sml" ;  *)
(* require "intern-s.sml" ;  *)
(* require "basis.__array" ;  *)

let intern_oriented_rule   = 1 ;;
let intern_unoriented_rule = 2 ;;
let intern_bool            = 3 ;;
let intern_true            = 4 ;;
let intern_false           = 5 ;;
let intern_unit            = 6 ;;
let intern_identity        = 7 ;;
let intern_trivial         = 8 ;;
let intern_and             = 9 ;;
let intern_or              = 10 ;;
let intern_equal           = 11 ;;
let intern_preceq          = 12 ;;
let intern_defined         = 13 ;;
let intern_all             = 14 ;;
let intern_exists          = 15 ;;
let intern_star            = 16 ;;
let intern_not             = 17 ;;
let intern_if              = 18 ;;
let intern_undef           = 19 ;;
let intern_set             = 20 ;;
let intern_cons            = 21 ;;
let intern_nil             = 22 ;;
let intern_attr            = 23 ;;
let intern_ac              = 24 ;;
let intern_a               = 25 ;;
let intern_c               = 26 ;;
let intern_epo             = 27 ;;
let intern_eq              = 28 ;;
let intern_derive          = 29 ;;
let intern_to              = 30 ;;
let intern_eto             = 31 ;;
let intern_po              = 32 ;;
let intern_smi             = 33 ;;
let intern_csmi            = 34 ;;
let intern_omi             = 35 ;;
let intern_comi            = 36 ;;
let intern_smd             = 37 ;;
let intern_csmd            = 38 ;;
let intern_omd             = 39 ;;
let intern_comd            = 40 ;;
let intern_mi              = 41 ;;
let intern_cmi             = 42 ;;
let intern_md              = 43 ;;
let intern_cmd             = 44 ;;
let intern_solved          = 45 ;;
let intern_t               = 46 ;;
let intern_goal            = 47 ;;
let intern_node            = 48 ;;
let intern_branch          = 49 ;;
let intern_subterm         = 50 ;;
let intern_pattern         = 51 ;;
let intern_down            = 52 ;;
let intern_top_goal        = 53 ;;
let intern_top_node        = 54 ;;
let intern_top_branch      = 55 ;;
let intern_top_subterm     = 56 ;;
let intern_top_pattern     = 57 ;;
let intern_top_down        = 58 ;;
let intern_subgoals        = 59 ;;
let intern_decomp          = 60 ;;
let intern_less            = 61 ;;
let intern_plus            = 62 ;;
let intern_slash           = 63 ;;
let intern_percent         = 64 ;;
let intern_minus           = 65 ;;
let intern_size            = 66 ;;
let intern_char            = 67 ;;
let intern_concat          = 68 ;;
let intern_chr             = 69 ;;
let intern_asc             = 70 ;;
let intern_def             = 71 ;;
let intern_default         = 72 ;;
let intern_lambda          = 73 ;;
let intern_fn              = 74 ;;
let intern_apply           = 75 ;;
let intern_nat_plus        = 76 ;;
let intern_nat_minus       = 77 ;;
let intern_nat_times       = 78 ;;
let intern_nat_divide      = 79 ;;
let intern_nat_less        = 80 ;;
let intern_nat_mod         = 81 ;;
let intern_rat_plus        = 82 ;;
let intern_rat_minus       = 83 ;;
let intern_rat_times       = 84 ;;
let intern_rat_divide      = 85 ;;
let intern_rat_less        = 86 ;;
let intern_rat_mod         = 87 ;;
let intern_rat_to_nat      = 88 ;;
let intern_nat_to_rat      = 89 ;;
let intern_implies         = 90 ;;
let intern_fix             = 91 ;;

let next = ref 92 ;;

let count () = (!next)-1;;

let decode_array = ref (Array.init (!next)
                                (fun (x) ->
                                  match x
                                    with
                                     | 1  -> "->"
                                     | 2  -> "="
                                     | 3  -> "Bool"
                                     | 4  -> "True"
                                     | 5  -> "False"
                                     | 6  -> "Unit"
                                     | 7  -> "Identity"
                                     | 8  -> "Trivial"
                                     | 9  -> "&"
                                     | 10 -> "|"
                                     | 11 -> "=="
                                     | 12 -> "preceq"
                                     | 13 -> "defined"
                                     | 14 -> "ALL"
                                     | 15 -> "EXISTS"
                                     | 16 -> "*"
                                     | 17 -> "not"
                                     | 18 -> "if"
                                     | 19 -> "Undef"
                                     | 20 -> "SET"
                                     | 21 -> "Cons"
                                     | 22 -> "Nil"
                                     | 23 -> "attr"
                                     | 24 -> "AC"
                                     | 25 -> "A"
                                     | 26 -> "C"
                                     | 27 -> "EPO"
                                     | 28 -> "EQ"
                                     | 29 -> "DERIVE"
                                     | 30 -> "TO"
                                     | 31 -> "ETO"
                                     | 32 -> "PO"
                                     | 33 -> "SMI"
                                     | 34 -> "CSMI"
                                     | 35 -> "OMI"
                                     | 36 -> "COMI"
                                     | 37 -> "SMD"
                                     | 38 -> "CSMD"
                                     | 39 -> "OMD"
                                     | 40 -> "COMD"
                                     | 41 -> "MI"
                                     | 42 -> "CMI"
                                     | 43 -> "MD"
                                     | 44 -> "CMD"
                                     | 45 -> "Solved"
                                     | 46 -> "T"
                                     | 47 -> "goal"
                                     | 48 -> "node"
                                     | 49 -> "branch"
                                     | 50 -> "subterm"
                                     | 51 -> "pattern"
                                     | 52 -> "down"
                                     | 53 -> "top_goal"
                                     | 54 -> "top_node"
                                     | 55 -> "top_branch"
                                     | 56 -> "top_subterm"
                                     | 57 -> "top_pattern"
                                     | 58 -> "top_down"
                                     | 59 -> "subgoals"
                                     | 60 -> "decomp"
                                     | 61 -> "<"
                                     | 62 -> "+"
                                     | 63 -> "/"
                                     | 64 -> "%"
                                     | 65 -> "-"
                                     | 66 -> "size"
                                     | 67 -> "char"
                                     | 68 -> "concat"
                                     | 69 -> "chr"
                                     | 70 -> "asc"
                                     | 71 -> "def"
                                     | 72 -> "default"
                                     | 73 -> "lambda"
                                     | 74 -> "fn"
                                     | 75 -> "apply"
                                     | 76 -> "nplus"
                                     | 77 -> "nminus"
                                     | 78 -> "ntimes"
                                     | 79 -> "ndivide"
                                     | 80 -> "nless"
                                     | 81 -> "nmod"
                                     | 82 -> "rplus"
                                     | 83 -> "rminus"
                                     | 84 -> "rtimes"
                                     | 85 -> "rdivide"
                                     | 86 -> "rless"
                                     | 87 -> "rmod"
                                     | 88 -> "ratnat"
                                     | 89 -> "natrat"
                                     | 90 -> "implies"
                                     | 91 -> "fix"
                                     | _  -> "")) ;;

let rec update n t = if n=0 then t else
                     update (n-1) (Rtrie.trieAdd t (Array.get (!decode_array) n) n) ;;

let encode_trie = ref (update ((!next)-1) (Rtrie.trieNew 0)) ;;

let decode i = (Array.get (!decode_array) i) ;;

let intern s =
    (*let _ = print_string ("Interning " ^ s) in*)
    let r = Rtrie.trieFind (!encode_trie) s in
    (*let _ = print_string (" found " ^ (string_of_int r) ^ "\n") in*)
        if r=0 then
            (((decode_array := (Array.init ((!next)+1)
                 (fun (x) -> if x < (!next) then (Array.get (!decode_array) x) else s)))) ;
             (encode_trie := Rtrie.trieAdd (!encode_trie) s (!next)) ;
              next := (!next)+1 ; (!next)-1)
        else
            r ;;




