open Util
open Ast

let check_vars (f : Ast.formula) : unit =
	let vit = Ast.vars_in_formula f in
	Printf.printf "Variables:\n";
	List.iter (Printf.printf "%s ") vit;
	print_newline ();
	let values = Ast.values_in_formula f in
	Printf.printf "Values:\n";
  SetMap.iter_all (Printf.printf "%s: %s\n") values;
