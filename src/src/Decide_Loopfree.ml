
let loop_freedom edge_pol pol topo _ (*out_edge_pol *) = 
  let open Decide_Ast in 
  let open Decide_Base in 
  let open Decide_Util in 
  let open Decide_Deriv in 
  disable_dexter_opt ();
  if set_univ [Term.values edge_pol; Term.values pol; Term.values topo]
  then 
    begin
      let p_t = (Term.make_times [pol;topo]) in
      let trm = Term.make_times [edge_pol; Term.make_star p_t] in 
      let pset = Term.one_dup_e_matrix trm in 
      let inner_term = Term.make_times [p_t; Term.make_star p_t] in
      let inner_e = Term.one_dup_e_matrix inner_term in 
      Printf.printf "Beginning fold\n%!";
      Printf.printf "The matrix is this big: %u\n%!" 
	(Decide_Base.Base.Set.cardinal pset);
      let cntr = ref 0 in 
      Base.Set.fold_points 
	(fun pt acc -> 
	  let beta = Base.point_rhs pt in
	  let beta_beta = 
	    Decide_Base.Base.complete_test_to_point beta beta in 
	  if (not (Base.Set.contains_point inner_e beta_beta))
	  then acc
	  else (Printf.printf 
		  "Bad! Circular path found: Entering at %s, we loop on %s\n"
		  (Base.complete_test_to_string (Base.point_lhs pt))
		  (Base.complete_test_to_string (Base.point_rhs pt));
		false)
	) pset true
    end
  else true
 
