
open Ast
open Base
open Util
open Spines

(* Derivatives                                                       *)

(* D(e) is represented as a sparse At x At matrix of expressions     *)
(* D(e)_{ap}(e) is the derivative of e with respect to ap dup        *)
(* where a is a complete test (atom) and p is a complete assignment  *)

(* E(e) is represented as a sparse At x At matrix over 0,1           *)
(* E(e)_{ap}(e) = 1 iff ap <= e                                      *)

(* I(e) is the diagonal matrix with diagonal elements e, 0 elsewhere *)

(* D(e1 + e2) = D(e1) + D(e2)                                        *)
(* this is still just map + fold*)
(* D(e1e2) = D(e1)I(e2) + E(e1)D(e2)                                 *)
(* this is more complicated. 
   D([e1;...;en]) = D(e1)*(I(e2),...,I(en))
                    + E(e1)*D(e2)*(I(e3),...,I(en))
                    + E(e1 e2)*D(e2)*(I(e4),...,I(en))
                    + E(e1 ... en)*D(en)
*)
(* D(e* ) = E(e* )D(e)I(e* )                                            *)
(* D(a) = D(p) = 0                                                   *)
(* D(dup) = I(a) (diagonal matrix where (a,a) = a)                   *)

(* E(e1 + e2) = E(e1) + E(e2)                                        *)
(* just fold addition over the set *)
(* E(e1e2) = E(e1)E(e2)                                              *)
(* just fold multiplication over the list. *)
(* also remember to map e1 -> E(e1) *)
(* E(e* ) = E(e)*                                                     *)
(* E(a) = 1 in diag element ap_a, 0 elsewhere                        *)
(* E(p) = 1 in p-th column, 0 elsewhere                              *)
(* E(dup) = 0                                                        *)

(* the base-case for + is 0; the base-case for * is 1.*)

module Deriv = functor(UDesc: UnivDescr) -> struct 

  module U = Univ(UDesc)

  type deriv_term = 
    | Spine of Term.term
    | BetaSpine of Term.term * TermSet.t
    | Zero
	
  let deriv_term_to_term = function 
    | Spine e -> e
    | BetaSpine (b,e) -> Term.Times[b;Term.Plus e]
    | Zero -> Term.Zero

  let calc_deriv_main all_spines (e : Term.term) = 
    let d,pts = TermSet.fold 
      (fun spine_pair (acc,set_of_points) -> 
	(* pull out elements of spine pair*)
	let e1,e2 = match spine_pair with 
	  | Term.Times [lspine;rspine] -> lspine,rspine
	  | _ -> failwith "Dexter LIES" in
	
	(* calculate e of left spine*)
	let corresponding_E = U.Base.Set.of_term e1 in
	let er_E = U.Base.Set.of_term (Ast.one_dups e2) in
	let er_E' = U.Base.Set.fold 
	  (fun base acc -> U.Base.Set.add (U.Base.project_lhs base) acc)
	  er_E U.Base.Set.empty in
	let e_where_intersection_is_present =  U.Base.Set.mult corresponding_E er_E' in
	let internal_matrix_ref point = 
	  if U.Base.Set.contains_point e_where_intersection_is_present point then
	    (Spine e2)
	  (* mul_terms (U.Base.test_of_point point) e2 *)
	  else 
	    Zero in 
	let more_points = 
	  U.Base.Set.union set_of_points e_where_intersection_is_present in
	
	(fun point -> 
	  match (internal_matrix_ref point) with 
	    | Zero -> acc point
	    | Spine e' -> TermSet.add e' (acc point)
	    | BetaSpine (b,e') -> failwith "this can't be produced"),
	more_points)
      (Hashtbl.find all_spines e) 
      ((fun _ -> TermSet.empty), U.Base.Set.empty) in
    (fun point -> BetaSpine (U.Base.test_of_point_right point, d point)), pts
      
  let calc_deriv_main = Util.memoize_on_arg2 calc_deriv_main
    
    
  let calculate_deriv all_spines (e : deriv_term) = 
    match e with 
      | (Zero | Spine Term.Zero) -> (fun _ -> Zero), U.Base.Set.empty
      | BetaSpine (beta, spine_set) -> 
	let d,points = 
	  TermSet.fold 
	    ( fun sigma (acc_d,acc_points) -> 
	      let d,points = calc_deriv_main all_spines sigma in
	      (fun point -> 
		match (d point) with 
		  | Zero -> acc_d point
		  | Spine _ -> failwith "why did deriv produce a Spine and not a BetaSpine?"
		  | BetaSpine (_,s) -> TermSet.union s (acc_d point)
	      ),(U.Base.Set.union acc_points points)
	    ) spine_set ((fun _ -> TermSet.empty), U.Base.Set.empty) in
	let points = U.Base.Set.filter_alpha points beta in
	(fun delta_gamma -> 
	  let delta = U.Base.test_of_point_left delta_gamma in
	  let gamma = U.Base.test_of_point_right delta_gamma in
	  if beta = delta
	  then BetaSpine(gamma,d delta_gamma)
	  else Zero),points
      | Spine e -> calc_deriv_main all_spines e
	
  let calculate_deriv = Util.memoize_on_arg2 calculate_deriv
    
  module WorkList = WorkList(struct 
    type t = (deriv_term * deriv_term) 
    let compare = Pervasives.compare
  end)
    
  let get_state,update_state,print_states = 
    Dot.init (fun a -> not (U.Base.Set.is_empty a))
  (* (fun _ _ _ _ -> true,true,1,1), (fun _ _ _ _ _ -> ()), (fun _ -> ()) *)
      
    
end
    
let check_equivalent (t1:term) (t2:term) : bool = 
  
  (* TODO: this is a heuristic.  Which I can spell, hooray.  *)
  let univ = StringSetMap.union (values_in_term t1) (values_in_term t2) in 
  let univ = List.fold_left (fun u x -> StringSetMap.add x "☃" u) univ (StringSetMap.keys univ) in
  let module UnivDescr = struct
    type field = string
    type value = string
    module FieldSet = Set.Make(String)
    module ValueSet = StringSetMap.Values
    let field_compare = Pervasives.compare
    let value_compare = Pervasives.compare
    let all_fields = 
      (* TODO: fix me when SSM is eliminated *)
      List.fold_right FieldSet.add (StringSetMap.keys univ) FieldSet.empty
    let all_values f = 
      try 
	StringSetMap.find_all f univ
      with Not_found -> 
	ValueSet.empty
    let field_to_string x = x
    let value_to_string x = x
    let field_of_id x = x
    let value_of_id x = x
    let value_of_string x = x
  end in   

  let module InnerDeriv = Deriv(UnivDescr) in
  let open InnerDeriv in
  let uf_eq,uf_find,uf_union = 
    Util.init_union_find ()  in
  
  let spines_t1 = allLRspines t1 in
  let spines_t2 = allLRspines t2 in
    
  let rec main_loop work_list = 
    if WorkList.is_empty work_list
    then 
      (print_states (); true)
    else
      let q1,q2 = WorkList.hd work_list in
      let q1_term,q2_term = deriv_term_to_term q1, deriv_term_to_term q2 in
      let rest_work_list = WorkList.tl work_list in
      let q1_E = U.Base.Set.of_term q1_term in
      let q2_E = U.Base.Set.of_term q2_term in
      if not (U.Base.Set.equal q1_E q2_E)
      then false
      else
	let u,f = uf_find(q1),uf_find(q2) in
	if  uf_eq u f  
	then main_loop rest_work_list
	else 
	  (let _ = uf_union u f in
	   let (dot_bundle : Dot.t) = 
	     get_state 
	       (deriv_term_to_term q1) (deriv_term_to_term q2) q1_E q2_E in
	   let q1_matrix,q1_points = calculate_deriv spines_t1 q1 in 
	   let q2_matrix,q2_points = calculate_deriv spines_t2 q2 in 
	   let numpoints = ref 0 in
	   let work_list = U.Base.Set.fold_points
	     (fun pt expanded_work_list -> 
	       numpoints := !numpoints + 1;
	       let q1' = q1_matrix pt in
	       let q2' = q2_matrix pt in
	       let q1'_term = deriv_term_to_term q1' in 
	       let q2'_term = deriv_term_to_term q2' in
	       update_state 
		 dot_bundle 
		 q1'_term
		 q2'_term
		 (U.Base.Set.of_term q1'_term)
		 (U.Base.Set.of_term q2'_term);
	       WorkList.add (q1',q2')
		 expanded_work_list
	     )
	     (U.Base.Set.union q1_points q2_points) rest_work_list in
	   main_loop work_list) in
  main_loop (WorkList.singleton (Spine t1,Spine t2))
