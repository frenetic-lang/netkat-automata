
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

let rec normalize (h : StringSetMap.t) (t : term) : (StringSetMap.t * StringSetMap.t) list =
  match t with 
  | Assg (var, value) ->
      let h1 = StringSetMap.empty in
      let h2 = StringSetMap.add var value (StringSetMap.empty) in
      [(h1,h2)]
  | Test (var, value) ->
          let h1 = StringSetMap.add var value (StringSetMap.empty) in
      let h2 = StringSetMap.empty in
      [(h1,h2)]
  | Dup -> []
  | Plus x -> []
  | Times x -> []
  | Not x -> []
  | Star x -> []
  | Zero -> []
  | One -> [(h,StringSetMap.empty)]
		 