(******************************************************************************
 *
 * REWRITELIB
 *
 * trie.mli
 *
 * This file contains the trie data structure implementation
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

(* require "trie-s.sml" ;  *)
(* require "basis.__array" ;  *)

type 'a trieRec = Result of (char list) * 'a
                  | DefaultMapping of ('a trieRec) array
                  | ResultMapping of ('a * (('a trieRec) array))
                  | Default

type 'a trie = 'a * ('a trieRec);;

let trieNew x = (x,Default);;

let replace a n v =
    Array.init (Array.length a)
               (fun x -> if x=n then v else (Array.get a x));;

let ord2 x = (int_of_char x)-32;;

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let rec tdomain s r = match r with
  | (Result (sl,_)) -> [(s ^ (string_of_chars sl))]
  | Default -> []
  | (DefaultMapping t) -> tdomainList s 95 t
  | (ResultMapping (r,t)) -> s::(tdomainList s 95 t)
and tdomainList s n t =
    if n>=0 then
        (tdomain (s ^ (String.make 1 (char_of_int (n+32)))) (Array.get t n))@(tdomainList s (n-1) t)
    else
        ([]);;

let trieDomain (_,t) = tdomain "" t

let rec trieA t l vl = match t,l with
  | (Default,l) -> Result (l,vl)
  | ((DefaultMapping a),[]) -> ResultMapping (vl,a)
  | ((ResultMapping (_,a)),[]) -> ResultMapping (vl,a)
  | ((ResultMapping (v,a)),(f::r)) ->
    ResultMapping (v,replace a (ord2 f) (trieA (Array.get a (ord2 f)) r vl))
  | ((DefaultMapping a),(f::r)) ->
    DefaultMapping (replace a (ord2 f) (trieA (Array.get a (ord2 f)) r vl))
  | ((Result ([],v)),[]) -> Result ([],vl)
  | ((Result (f::r,v)),[]) ->
    ResultMapping (vl,Array.init 96 (fun (x) -> if x=ord2 f then Result (r,v) else Default))
  | ((Result (nil,v)),(f::r)) ->
    ResultMapping (v,Array.init 96 (fun (x) -> if x=ord2 f then Result (r,vl) else Default))
  | ((Result (f1::r1,v)),(f2::r2)) ->
    if f1=f2 then
        DefaultMapping ( Array.init 96
            (fun (x) -> if x=ord2 f1 then trieA (Result (r1,v)) r2 vl
                        else Default))
    else
        DefaultMapping ( Array.init 96
            (fun (x) -> if x=ord2 f1 then Result (r1,v)
                        else if x=ord2 f2 then Result (r2,vl)
                            else Default));;

let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) [];;

let trieAdd (def,t) s v = (def,trieA t (explode s) v);;

let rec trieF t def l2 = match t with
  | Default -> def
  | (Result (l1,vl)) -> if l1=l2 then vl else def
  | (DefaultMapping a) -> (match l2 with
                          | (f::r) -> trieF (Array.get a (ord2 f)) def r
                          | [] -> def)
  | (ResultMapping (v,a)) -> match l2 with
                             | [] -> v
                             | (f::r) -> trieF (Array.get a (ord2 f)) def r;;

let trieFind (def,a) s = trieF a def (explode s);;




