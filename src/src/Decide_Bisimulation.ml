
open Decide_Base
open Decide_Util

    
module U = Decide_Base
module Ast = Decide_Ast
module Deriv = Decide_Deriv
  
module WorkList = WorkList(struct 
  type t = (Deriv.DerivTerm.t * Deriv.DerivTerm.t) 
    let compare = (fun (a1,b1) (a2,b2) -> 
      match (Deriv.DerivTerm.compare a1 a2) with 
	| 0 -> (Deriv.DerivTerm.compare b1 b2)
	| k -> k)
  end)
    
  let print_wl_pair (a,b)= Printf.sprintf "%s\n%s" (Deriv.DerivTerm.to_string a) (Deriv.DerivTerm.to_string b)
    
  let get_state,update_state,print_states = 
    (* Decide_Dot.init Deriv.DerivTerm.to_string (fun a -> not (U.Base.Set.is_empty a)) *)
    (fun _ _ _ _ -> true,true,1,1), (fun _ _ _ _ _ -> ()), (fun _ -> ()) 
      
      
  let check_equivalent (t1: Ast.Term.t) (t2: Ast.Term.t) : bool = 

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
	let q1_E = Deriv.DerivTerm.run_e q1 in 
	let q2_E = Deriv.DerivTerm.run_e q2 in 
	Printf.printf "q1 term:%s\nq2 term:%s\nq1 E: %s\nq2 E: %s\n" 
	  (Decide_Deriv.DerivTerm.to_string q1)
	  (Decide_Deriv.DerivTerm.to_string q2)
	  (U.Base.Set.to_string q1_E)
	  (U.Base.Set.to_string q2_E);
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
	     let q1_matrix,q1_points = Deriv.DerivTerm.run_d q1 in 
	     let q2_matrix,q2_points = Deriv.DerivTerm.run_d q2 in 
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
		   (Deriv.DerivTerm.run_e q1')
		   (Deriv.DerivTerm.run_e q2');
		 WorkList.add (q1',q2')
		   expanded_work_list
	       )
	       (U.Base.Set.union q1_points q2_points) rest_work_list in
	     main_loop work_list) in
    Printf.printf "Beginning term conversion\n%!" ;
    let t2 = Deriv.DerivTerm.make_term t2 in
    Printf.printf "rhs term complete\n%!";
    let t1 = Deriv.DerivTerm.make_term t1 in 
    Printf.printf "Term construction complete\n%!";
    let ret = main_loop (WorkList.singleton (t1,t2)) in
    Decide_Util.print_debugging_info (); 
    ret
