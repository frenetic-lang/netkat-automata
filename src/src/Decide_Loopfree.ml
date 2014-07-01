
let loop_freedom edge_pol pol topo _ (*out_edge_pol *) = 
  let open Decide_Ast in 
  let open Decide_Base in 
  let open Decide_Util in 
  let open Decide_Deriv in 
  if set_univ [Term.values edge_pol; Term.values pol; Term.values topo]
  then 
    begin
      let p_t = (Term.make_times [pol;topo]) in
      let trm = Term.make_times [edge_pol; Term.make_star p_t] in 
      let pset = Term.one_dup_e_matrix trm in 
      Base.Set.fold_points 
	(fun pt acc -> 
	  let beta_t = Term.of_complete_test (Base.point_rhs pt) in
	  let newterm = 
	    (Term.make_times 
	       [beta_t; p_t; Term.make_star p_t ; beta_t]) in
	  let em = Term.one_dup_e_matrix newterm in 
	  if (Base.Set.is_empty em)
	  then acc
	  else (Printf.printf 
		  "Bad! Circular path found: Entering at %s, we loop on %s\n"
		  (Base.complete_test_to_string (Base.point_lhs pt))
		  (Base.complete_test_to_string (Base.point_rhs pt));
		false)
	) pset true
    end
  else true
 
