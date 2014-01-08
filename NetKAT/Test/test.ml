open Util
open Ast

module S = StringSetMap
module B = Base.BaseSet
type base = S.t * S.t

let display_term (t : term) =
  Printf.printf "%s\n" (Ast.term_to_string t)

let display_base (b : base) =
  Printf.printf "%s\n" (Base.base_to_string b)
  
let display_baseset (m : B.t) =
  print_endline (B.to_string m)

let display_binding (k,l) =
  let s = String.concat "," l in
  Printf.printf "%s:{%s} " k s

let test (f : Ast.formula) : unit =
  
  let f = Ast.simplify_formula f in
  Printf.printf "Original:\n%s\n" (Ast.formula_to_string f);
  let (s,t) = Ast.terms_in_formula f in
  let s = Ast.deMorgan (Ast.zero_dups s) in
  let t = Ast.deMorgan (Ast.zero_dups t) in
  let g = Eq (s,t) in
  Printf.printf "Simplified:\n%s\n" (Ast.formula_to_string g);
  
	(* let s = Ast.vars_in_formula f in                                                    *)
	(* Printf.printf "Variables: ";                                                        *)
	(* List.iter (Printf.printf "%s ") s;                                                  *)
	(* print_newline ();                                                                   *)
  
	(* print_values (Ast.values_in_formula f);                                             *)

  (* let (s,t) = Ast.terms_in_formula (Ast.simplify_formula f) in                        *)
  (* let (hs,ht) = (coterm s, coterm t) in                                               *)
  (* Printf.printf "# subterms s: %d\n" (Hashtbl.length hs);                             *)
  (* Printf.printf "%s\n" (Ast.coterm_to_string hs);                                     *)
  (* Printf.printf "# subterms t: %d\n" (Hashtbl.length ht);                             *)
  (* Printf.printf "%s\n" (Ast.coterm_to_string ht);                                     *)

  let univ = Normalize.universe f in
  let s = Normalize.normalize univ s in
  let t = Normalize.normalize univ t in
  Printf.printf "Normalized:\n";
  Printf.printf "lhs: ";
  display_baseset s;
  Eval.test_atomized_baseset univ s;
  Printf.printf "rhs: ";
  display_baseset t;
  Eval.test_atomized_baseset univ t;
  (* Printf.printf "Reduced:\n";             *)
  (* Printf.printf "lhs: ";                  *)
  (* display_baseset (Normalize.breakout s); *)
  (* Printf.printf "rhs: ";                  *)
  (* display_baseset (Normalize.breakout t); *)

  print_endline (if Eval.eval f then "true" else "false");
  print_newline ()
  