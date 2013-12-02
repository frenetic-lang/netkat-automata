
open Util
open Ast
open Term

(* Set dups to 0 and simplify, then represent as a sum of terms          *)
(* of the form tp, where t is a sequence of tests of the form x in A     *)
(* for a variable x and set of values A for that variable, where each    *)
(* variable occurs at most once in t, and p is a sequence of assignments *)
(* x := n, where each variable occurs at most once. This is a succinct   *)
(* representation of atoms and complete assignments.                     *)

(* Each tp is represented as a pair (t,p) of SetMaps. *)

(* UNDER CONSTRUCTION *)

module SetMap = SetMapF (String) (String)

let rec normalize (h : SetMap.t) (t : term) : (SetMap.t * SetMap.t) list =
	match t with 
	| Assg (var, value) ->
		  let h1 = SetMap.make() in
		  let h2 = SetMap.add var value (SetMap.make()) in
		  [(h1,h2)]
  | Test (var, value) ->
		  let h1 = SetMap.add var value (SetMap.make()) in
      let h2 = SetMap.make() in
      [(h1,h2)]
  | Plus x -> []
  | Times x -> []
  | Not x -> []
  | Star x -> []
  | Zero -> []
  | One -> [(h,SetMap.make())]
		 