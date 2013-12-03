open Util
open Ast

let test (f : Ast.formula) : unit =
  
  print_endline (Ast.formula_to_string f);
  
	let vit = Ast.vars_in_formula f in
	Printf.printf "Variables: ";
	List.iter (Printf.printf "%s ") vit;
	print_newline ();
  
	let values = Ast.values_in_formula f in
	Printf.printf "Values:\n";
  StringSetMap.iter_all (Printf.printf "%s: %s\n") values;
  
  Printf.printf "%s\n\n" (Ast.formula_to_string (Ast.simplify_formula f))
