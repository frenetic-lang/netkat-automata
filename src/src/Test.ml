open Util
open Ast
open Base
open Bisimulation

module S = StringSetMap
type base = S.t * S.t

let display_term (t : term) =
  Printf.printf "%s\n" (Ast.term_to_string t)


let display_binding (k,l) =
  let s = String.concat "," l in
  Printf.printf "%s:{%s} " k s
  
let check (s : string) (b : bool) : unit =
  Printf.printf "Check %s: %s\n" s (if b then "true" else "false")

let test (f : Ast.formula) : unit =
  
  Printf.printf "Original:\n%s\n" (Ast.formula_to_string f);
  let f = Ast.simplify_formula f in
  Printf.printf "Simplified:\n%s\n" (Ast.formula_to_string f);
  let (s,t) = Ast.terms_in_formula f in
  let (s,t) = (Ast.deMorgan s,Ast.deMorgan t) in
  
  let u = Spines.rspines s in
  let v = Spines.rspines t in
  Printf.printf "LHS rspines:\n%s\n" (Ast.termset_to_string u);
  Printf.printf "RHS rspines:\n%s\n" (Ast.termset_to_string v);
  
  Printf.printf "Bisimulation result: %b\n"
    (Bisimulation.check_equivalent (Ast.deMorgan s) (Ast.deMorgan t))
