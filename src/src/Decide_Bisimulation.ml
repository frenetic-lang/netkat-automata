
open Decide_Ast
open Decide_Base
open Decide_Util

module Bisimulation = functor(UDesc: UnivDescr) -> struct 
    
  module U = Univ(UDesc)
  module Deriv = Decide_Deriv.Deriv(UDesc)
    
  module WorkList = WorkList(struct 
    type t = (Deriv.DerivTerm.t * Deriv.DerivTerm.t) 
    let compare = (fun (a1,b1) (a2,b2) -> 
      Pervasives.compare (Deriv.DerivTerm.compare a1 a2) (Deriv.DerivTerm.compare b1 b2))
  end)
    
  let get_state,update_state,print_states = 
    Decide_Dot.init Deriv.DerivTerm.to_string (fun a -> not (U.Base.Set.is_empty a))
  (* (fun _ _ _ _ -> true,true,1,1), (fun _ _ _ _ _ -> ()), (fun _ -> ()) *)
      
    
end
    
let check_equivalent (t1:term) (t2:term) : bool = 

  (* TODO: this is a heuristic.  Which I can spell, hooray.  *)
  let module UnivMap = Decide_Util.SetMapF (Decide_Ast.Term.Field) (Decide_Ast.Term.Value) in
  let univ = UnivMap.union (values_in_term t1) (values_in_term t2) in 
  let univ = List.fold_left (fun u x -> UnivMap.add x Term.Value.extra_val u) univ (UnivMap.keys univ) in
  let module UnivDescr = struct
    type field = Decide_Ast.Term.Field.t
    type value = Decide_Ast.Term.Value.t
    module FieldSet = Set.Make(Decide_Ast.Term.Field)
    module ValueSet = UnivMap.Values
    let all_fields = 
      (* TODO: fix me when SSM is eliminated *)
      List.fold_right FieldSet.add (UnivMap.keys univ) FieldSet.empty
    let all_values f = 
      try 
	UnivMap.find_all f univ
      with Not_found -> 
	ValueSet.empty
  end in   

  let module InnerBsm = Bisimulation(UnivDescr) in
  let open InnerBsm in
  let uf_eq,uf_find,uf_union = 
    let module UF = Decide_Util.UnionFind(Deriv.DerivTerm) in 
    UF.init_union_find ()  
  in
      
  let rec main_loop work_list = 
    if WorkList.is_empty work_list
    then 
      (print_states (); true)
    else
      let q1,q2 = WorkList.hd work_list in
      let rest_work_list = WorkList.tl work_list in
      let q1_E = Deriv.run_e q1 in 
      let q2_E = Deriv.run_e q2 in 
      if not (U.Base.Set.equal q1_E q2_E)
      then false
      else
	let u,f = uf_find(q1),uf_find(q2) in
	if  uf_eq u f  
	then main_loop rest_work_list
	else 
	  (let _ = uf_union u f in
	   let (dot_bundle : Decide_Dot.t) = 
	     get_state 
	       q1 q2 q1_E q2_E in
	   let q1_matrix,q1_points = Deriv.run_d q1 in 
	   let q2_matrix,q2_points = Deriv.run_d q2 in 
	   let numpoints = ref 0 in
	   let work_list = U.Base.Set.fold_points
	     (fun pt expanded_work_list -> 
	       numpoints := !numpoints + 1;
	       let q1' = q1_matrix pt in
	       let q2' = q2_matrix pt in
	       update_state 
		 dot_bundle 
		 q1'
		 q2'
		 (Deriv.run_e q1')
		 (Deriv.run_e q2');
	       WorkList.add (q1',q2')
		 expanded_work_list
	     )
	     (U.Base.Set.union q1_points q2_points) rest_work_list in
	   main_loop work_list) in
  Printf.printf "beginning bisimulation loop\n%!";
  let ret = main_loop (WorkList.singleton 
			 (Deriv.DerivTerm.make_term 
			    (Decide_Ast.deMorgan
			       (Decide_Ast.simplify t1)), 
			  Deriv.DerivTerm.make_term 
			    (Decide_Ast.deMorgan
			       (Decide_Ast.simplify t2)))) in 
  U.Base.Set.print_debugging_info (); 
  ret
