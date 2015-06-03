open Decide_Util

module SatChecker (DerivTerm : Decide_Deriv.DerivTerm) = struct
  module Ast = Decide_Ast
  module Deriv = Decide_Deriv
  module E = DerivTerm.EMatrix
  module D = DerivTerm.DMatrix

  module WorkList = WorkList(struct 
      type t = (DerivTerm.t * DerivTerm.t) 
      let compare = (fun (a1,b1) (a2,b2) -> 
          match (DerivTerm.compare a1 a2) with 
	  | 0 -> (DerivTerm.compare b1 b2)
	  | k -> k)
    end)

  let rec run_sat work_list =
    if WorkList.is_empty work_list
    then
      true
    else
      let s1,s2 = WorkList.hd work_list in
      Printf.printf "s1: %s\ns2: %s\n" (DerivTerm.to_string s1) (DerivTerm.to_string s2);
      let rest_work_list = WorkList.tl work_list in
      let s1_E = DerivTerm.get_e s1 in
      let s2_E = DerivTerm.get_e s2 in
      if (E.compare (E.intersection s1_E (E.complement s2_E)) E.empty) <> 0
      then false
      else
        let s1_D = DerivTerm.get_d s1 in
        let work_list = E.fold (D.points s1_D)
            ~init:rest_work_list
            ~f:(fun work_list pt ->
                let s1' = D.run s1_D pt in
                let s2' = DerivTerm.run_exact s2 pt in
                WorkList.add (DerivTerm.make_term s1', DerivTerm.make_term s2')
                  work_list)
        in
        run_sat work_list

  let check_sat t p =
    let t' = DerivTerm.make_term (Ast.TermSet.singleton t) in
    let p' = DerivTerm.make_term (Ast.TermSet.singleton (Ast.Path.translate p)) in
    run_sat (WorkList.singleton (t', p'))

  (* let check_sat t p = *)
  (*   let module Checker = Decide_Bisimulation.EquivChecker (DerivTerm) in *)
  (*   Checker.check_equivalent (Ast.Term.intersection *)
  (*                               (Ast.TermSet.of_list [t; Ast.Term.complement (Ast.Path.translate p)])) Ast.Term.zero *)
end
