open Decide_Util
open Decide_Ast
open Decide_Base
open Decide_Bisimulation

module S = StringSetMap
type base = S.t * S.t

let display_term (t : term) =
  Printf.printf "%s\n" (Decide_Ast.term_to_string t)


let display_binding (k,l) =
  let s = String.concat "," l in
  Printf.printf "%s:{%s} " k s
  
let check (s : string) (b : bool) : unit =
  Printf.printf "Check %s: %s\n" s (if b then "true" else "false")

let test (f : Decide_Ast.formula) : unit =
  
  Printf.printf "Original:\n%s\n" (Decide_Ast.formula_to_string f);
  let f = Decide_Ast.simplify_formula f in
  Printf.printf "Simplified:\n%s\n" (Decide_Ast.formula_to_string f);
  let (s,t) = Decide_Ast.terms_in_formula f in
  let (s,t) = (Decide_Ast.deMorgan s,Decide_Ast.deMorgan t) in
  
  let u = Decide_Spines.rspines s in
  let v = Decide_Spines.rspines t in
  Printf.printf "LHS rspines:\n%s\n" (Decide_Ast.termset_to_string u);
  Printf.printf "RHS rspines:\n%s\n" (Decide_Ast.termset_to_string v);
  
  Printf.printf "Bisimulation result: %b\n"
    (Decide_Bisimulation.check_equivalent (Decide_Ast.deMorgan s) (Decide_Ast.deMorgan t))
