(******************************************************************************
 *
 * REWRITELIB
 *
 * trie.mli
 *
 * Signature for TRIE structure
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

type 'a trie ;;

val trieNew: 'a -> ('a trie);;
val trieAdd: ('a trie) -> string -> 'a -> ('a trie);;
val trieFind: ('a trie) -> string -> 'a;;
val trieDomain: ('a trie) -> (string list);;





