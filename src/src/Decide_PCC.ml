
module D = Decide_Deriv.DerivTerm
module S = Sexplib.Sexp
       
let generate_certificate t1 t2 t1' t2' _ =
  let file = open_out "/tmp/fsm1.dot" in 
  Printf.fprintf file "%s\n" (Decide_Ast.Term.to_string t1);
  Printf.fprintf file "%s\n" (S.to_string (D.sexp_of_t t1'));
  Printf.fprintf file "*****\n";
  Printf.fprintf file "%s\n" (Decide_Ast.Term.to_string t2);
  Printf.fprintf file "%s\n" (S.to_string (D.sexp_of_t t2'))
