
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

  type e_matrix = | E_Matrix of (unit -> U.Base.Set.t)
  and d_matrix = | D_Matrix of (unit -> 
				((U.Base.point -> deriv_term) * U.Base.Set.t))
      
  and deriv_term = 
    | Spine of Term.term (* actual term *) * 
	(* for speedy Base.Set calculation *) e_matrix ref * 
      (* for speedy Deriv calculation*) d_matrix ref
    | BetaSpine of Term.term * TermSet.t * 
	(* for speedy Base.Set calculation *) e_matrix ref * 
	(* for speedy Deriv calculation*) d_matrix ref
    | Zero of e_matrix ref * d_matrix ref
	  
	
  (* transition function *)
  let to_term = function 
    | Spine (e,_,_) -> e
    | BetaSpine (b,e,_,_) -> Term.Times[b;Term.Plus e]
    | Zero _ -> Term.Zero

  let compare e1 e2 = 
    Pervasives.compare (to_term e1) (to_term e2)
      
      
  let run_e trm : U.Base.Set.t = match trm with 
    | (Spine(_,e,_) | Zero(e,_) | BetaSpine(_,_,e,_)) -> 
      (match (!e) with 
	| E_Matrix e -> e ())
	
  let run_d trm = match trm with 
    | (Spine(_,_,d) | Zero(_,d) | BetaSpine(_,_,_,d)) -> 
      (match (!d) with 
	| D_Matrix d -> d ())
	
  let default_e_matrix trm =
    (E_Matrix (fun _ -> match trm with 
      | (Spine (_,em,_) | BetaSpine (_,_,em,_) | Zero(em,_)) -> 
	let ret = ( U.Base.Set.of_term (to_term trm)) in
	em := (E_Matrix (fun _ -> ret));
	ret))
      
  let default_d_matrix = ref (fun _ _ -> failwith "dummy1")
    
  let default_e_zero = ref (E_Matrix (fun _ -> U.Base.Set.empty))
    
  let default_d_zero = ref (D_Matrix(fun _ -> (fun _ -> failwith "dummy2"),U.Base.Set.empty))
    
  let _ = default_d_zero := (D_Matrix (fun _ -> 
    (fun _ -> Zero(default_e_zero,default_d_zero)),
    U.Base.Set.empty))
    
  let make_spine all_spines e2 = 
    let em_fun = ref (E_Matrix (fun _ -> failwith "dummy3")) in 
    let d_fun = ref (D_Matrix (fun _ -> failwith "dummy4")) in 
    let ret = (Spine (e2,em_fun ,d_fun)) in 
    em_fun := (default_e_matrix ret);
    d_fun := ((!default_d_matrix) all_spines ret);
    ret
      
  let make_zero _ = 
    (Zero (default_e_zero ,default_d_zero))
      
  let make_betaspine all_spines beta tm = 
    let em_fun = ref (E_Matrix (fun _ -> failwith "dummy5")) in 
    let d_fun = ref (D_Matrix (fun _ -> failwith "dummy6")) in 
    let ret = BetaSpine (beta, tm,em_fun,d_fun) in
    em_fun := default_e_matrix ret;
    d_fun := !default_d_matrix all_spines ret;
    ret
      
      
  let calc_deriv_main all_spines (e : Term.term) : ((U.Base.point -> deriv_term) * U.Base.Set.t)  = 
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
	    make_spine all_spines e2
	  (* mul_terms (U.Base.test_of_point point) e2 *)
	  else 
	    make_zero ()
	in 
	let more_points = 
	  U.Base.Set.union set_of_points e_where_intersection_is_present in
	
	(fun point -> 
	  match (internal_matrix_ref point) with 
	    | Zero (_,_)-> acc point
	    | Spine (e',_,_) -> TermSet.add e' (acc point)
	    | BetaSpine (b,e',_,_) -> failwith "this can't be produced"),
	more_points)
      (Hashtbl.find all_spines e) 
      ((fun _ -> TermSet.empty), U.Base.Set.empty) in
    (fun point -> 
      make_betaspine all_spines (U.Base.test_of_point_right point) (d point)
    ), pts
      
      
  let calculate_deriv all_spines (e : deriv_term) = 
    match e with 
      | (Zero _ | Spine (Term.Zero,_,_)) -> 
	(fun _ -> Zero(default_e_zero, default_d_zero)), 
	U.Base.Set.empty
      | BetaSpine (beta, spine_set,_,_) -> 
	let d,points = 
	  TermSet.fold 
	    ( fun sigma (acc_d,acc_points) -> 
	      let d,points = calc_deriv_main all_spines sigma in
	      (fun point -> 
		match (d point) with 
		  | Zero _ -> acc_d point
		  | Spine _ -> failwith "why did deriv produce a Spine and not a BetaSpine?"
		  | BetaSpine (_,s,_,_) -> TermSet.union s (acc_d point)
	      ),(U.Base.Set.union acc_points points)
	    ) spine_set ((fun _ -> TermSet.empty), U.Base.Set.empty) in
	let points = U.Base.Set.filter_alpha points beta in
	(fun delta_gamma -> 
	  let delta = U.Base.test_of_point_left delta_gamma in
	  let gamma = U.Base.test_of_point_right delta_gamma in
	  if beta = delta
	  then 
	    make_betaspine all_spines gamma (d delta_gamma)
	  else Zero(default_e_zero,default_d_zero)),points
      | Spine (e,_,_) -> 
	calc_deriv_main all_spines e
	  
  let _ = default_d_matrix := (fun asp trm -> 
    D_Matrix (fun _ -> 
      match trm with 
	| (Spine (_,_,dm) | BetaSpine (_,_,_,dm) | Zero(_,dm)) -> 
	  let ret = calculate_deriv asp trm in
	  dm := D_Matrix((fun _ -> ret));
	  ret)) 
    
  let make_deriv_term = make_spine
end
