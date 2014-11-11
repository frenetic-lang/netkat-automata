
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

  let check_equivalent (t1: Ast.Term.t) (t2: Ast.Term.t) : bool = 
    
    let module UF = Decide_Util.UnionFind(Deriv.DerivTerm) in
    let uf = UF.create ()
    in
    
    let rec main_loop work_list = 
      if WorkList.is_empty work_list
      then
	true
      else
	let q1,q2 = WorkList.hd work_list in
	let rest_work_list = WorkList.tl work_list in
	let q1_E = Deriv.DerivTerm.run_e q1 in 
	let q2_E = Deriv.DerivTerm.run_e q2 in 
	if not (U.Base.Set.equal q1_E q2_E)
	then false
	else
	  let u,f = UF.find uf q1, UF.find uf q2 in
	  if  UF.eq uf u f
	  then main_loop rest_work_list
	  else 
	    (let _ = UF.union uf u f in
	     let q1_matrix,q1_points = Deriv.DerivTerm.run_d q1 in 
	     let q2_matrix,q2_points = Deriv.DerivTerm.run_d q2 in 
	     let work_list = U.Base.Set.fold_points
	       (fun pt expanded_work_list -> 
		 let q1' = q1_matrix pt in
		 let q2' = q2_matrix pt in
		 WorkList.add (q1',q2')
		   expanded_work_list
	       )
	       (U.Base.Set.union q1_points q2_points) rest_work_list in
	     main_loop work_list) in

    let t2' = Deriv.DerivTerm.make_term t2 in

    let t1' = Deriv.DerivTerm.make_term t1 in 

    let ret = main_loop (WorkList.singleton (t1',t2')) in
    (* Decide_PCC.generate_certificate t1 t2 t1' t2' (uf_eq, uf_find, uf_union); *)
    Decide_Util.print_debugging_info ();
    ret
