
open Decide_Base
open Decide_Util

  module TermMap = Decide_Ast.TermMap
  module TermSet = Decide_Ast.TermSet
  module U = Decide_Base
  type spines_map = Decide_Ast.TermPairSet.t TermMap.t

  open Decide_Ast

  module rec DerivTerm : sig
    type d_matrix = ((U.Base.point -> t) * U.Base.Set.t)
    and e_matrix = U.Base.Set.t
    and t = 
      | Spine of Term.t * (unit -> e_matrix) * (unit -> d_matrix)
      | BetaSpine of U.Base.complete_test * TermSet.t * (unit-> e_matrix) * (unit -> d_matrix)
      | Zero of (unit -> e_matrix) * (unit -> d_matrix)

    val to_string : t -> string 
    val compare : t -> t -> int
    val make_term : Decide_Ast.term -> t
    val zero : t
    val make_spine : spines_map -> Term.t -> t
    val make_betaspine : spines_map -> 
      U.Base.complete_test -> TermSet.t -> t
    val default_d_matrix : (spines_map -> t -> (unit -> d_matrix)) ref
  end = struct 

    type d_matrix = ((U.Base.point -> t) * U.Base.Set.t)
    and e_matrix = U.Base.Set.t
    and t = 
      | Spine of Term.t * (unit -> e_matrix) * (unit -> d_matrix)
      | BetaSpine of U.Base.complete_test * TermSet.t * (unit-> e_matrix) * (unit -> d_matrix)
      | Zero of (unit -> e_matrix) * (unit -> d_matrix)

    let compare e1 e2 = 
      match e1,e2 with 
	| (Zero _,_) -> -1
	| (Spine _, BetaSpine _) -> -1 
	| (BetaSpine _, Spine _) -> 1
	| (_,Zero _) -> 1
	| (Spine (tm1,_,_), Spine (tm2,_,_)) -> 
	  Term.compare tm1 tm2
	| (BetaSpine (b1,s1,_,_),BetaSpine (b2,s2,_,_)) -> 
	  (match U.Base.compare_complete_test b1 b2 with 
	    | -1 -> -1
	    | 1 -> 1
	    | 0 -> TermSet.compare s1 s2
	    | _ -> failwith "value out of range for compare"
	  )
  
    let default_d_matrix = ref (fun _ _ -> failwith "dummy1")

    let default_e_matrix trm =
      match trm with
	| Spine (tm,_,_) ->
	  (Decide_Ast.get_cache tm).e_matrix
	| BetaSpine (beta,ts,em,_) ->
	  let valu = ref None in 
	  (fun _ -> 
	    match !valu with 
	      | Some e -> e
	      | None -> 
		let ret = 
		  (U.Base.Set.filter_alpha
		     (TermSet.fold
			(fun t -> U.Base.Set.union ((get_cache t).e_matrix ()) )
			ts U.Base.Set.empty)
		     beta) in 
		valu := Some ret;
		ret)
	| Zero(em,_) -> (fun _ -> U.Base.Set.empty)
      
    let make_zero = 
      let e_zero = (fun _ -> U.Base.Set.empty) in
      let rec d_zero = (fun _ -> (fun _ -> Zero(e_zero,d_zero)),U.Base.Set.empty) in 
      (fun _ -> Zero (e_zero,d_zero))

    let zero = make_zero ()
	
    let make_spine (all_spines : spines_map) tm = 
      
      (*what I wish I could write: 
	let rec ret = Spine(tm, default_e_matrix ret, !default_d_matrix all_spines ret) in ret
      *)
      
      let e_matrix_option = ref None in 
      let d_matrix_option = ref None in 
      let e_matrix_backpatch = 
	(fun _ -> match !e_matrix_option with None -> failwith "1" | Some e -> e ()) in 
      let d_matrix_backpatch = 
	(fun _ -> match !d_matrix_option with None -> failwith "1" | Some d -> d ()) in      

      let rec ret = Spine (tm, e_matrix_backpatch, d_matrix_backpatch) in 

      e_matrix_option := Some (default_e_matrix ret);
      d_matrix_option := Some (!default_d_matrix all_spines ret);
      ret

    let make_betaspine (all_spines : spines_map) beta tms = 
      let e_matrix_option = ref None in 
      let d_matrix_option = ref None in 
      let e_matrix_backpatch = 
	(fun _ -> match !e_matrix_option with None -> failwith "1" | Some e -> e ()) in 
      let d_matrix_backpatch = 
	(fun _ -> match !d_matrix_option with None -> failwith "1" | Some d -> d ()) in      

      let rec ret = BetaSpine (beta, tms, e_matrix_backpatch, d_matrix_backpatch) in

      e_matrix_option := Some (default_e_matrix ret);
      d_matrix_option := Some (!default_d_matrix all_spines ret);

      ret

    let make_term = (fun (e : Decide_Ast.term) -> 
      make_spine (Decide_Ast.allLRspines e) e)

    let to_string = function 
      | Zero _ -> "drop"
      | Spine (t,_,_) -> Decide_Ast.Term.to_string t
      | BetaSpine (b,t,_,_) -> 
	Printf.sprintf "%s;(%s)" (U.Base.complete_test_to_string b) 
	  (TermSet.fold 
	     (fun x acc -> 
	       if acc = "" 
	       then (Decide_Ast.Term.to_string x)
	       else Printf.sprintf "%s + %s" 
		 (Decide_Ast.Term.to_string x) acc) t "")
      
  end
  and DerivTermSet : sig
    include Set.S
  end with type elt = DerivTerm.t = struct
    include Set.Make(struct 
      type t = DerivTerm.t
      let compare = DerivTerm.compare
    end)
  end

  open DerivTerm
    
      
  let run_e trm : U.Base.Set.t = match trm with 
      (Spine(_,e,_) | Zero(e,_) | BetaSpine(_,_,e,_)) -> e ()
	
  let run_d trm = match trm with 
      (Spine(_,_,d) | Zero(_,d) | BetaSpine(_,_,_,d)) -> d ()

  let laure_optimization e2 = 
    let er_E = (get_cache e2).one_dup_e_matrix () in
    U.Base.Set.fold 
      (fun base acc -> U.Base.Set.add (U.Base.project_lhs base) acc)
      er_E U.Base.Set.empty 
    
      
  let calc_deriv_main all_spines (e : Term.t) : ((U.Base.point -> t) * U.Base.Set.t)  = 
    let d,pts = Decide_Ast.TermPairSet.fold 
      (fun (e1,e2) (rest_d,set_of_points) -> 
	
	(* calculate e of left spine*)
	let lhs_E = (get_cache e1).e_matrix () in
	
	(* filter by Laure's algorithm of right spine *)
	let filtered_e =  
	  U.Base.Set.mult lhs_E (laure_optimization e2) in

	(* the D function for this pair *)
	let internal_matrix_ref point = 
	  if U.Base.Set.contains_point 
	    filtered_e point then
	    make_spine all_spines e2
	  else 
	    zero
	in 

	(* all points on which above function is non-Zero *)
	let more_points = 
	  U.Base.Set.union set_of_points filtered_e in
	
	(* compose this spine's D matrix with the other spines' D matrices *)
	(fun point -> 
	  match (internal_matrix_ref point) with 
	    | Zero (_,_)-> rest_d point
	    | Spine (e',_,_) -> TermSet.add e' (rest_d point)
	    | BetaSpine (b,e',_,_) -> failwith "this can't be produced"),
	more_points)

      (TermMap.find e all_spines)
      
      ((fun _ -> TermSet.empty), U.Base.Set.empty) in
    
    (* multiply the sum of spines by Beta *)
    (fun point -> 
      make_betaspine all_spines (U.Base.point_rhs point) (d point)
    ), pts
     
  let _ = ()

  let calc_deriv_main = Decide_Ast.memoize_on_arg2 calc_deriv_main
      
  let calculate_deriv all_spines (e : t) : ((U.Base.point -> t) * U.Base.Set.t) = 
    match e with 
      | Zero _ -> 
	(fun _ -> zero ), U.Base.Set.empty
      | BetaSpine (beta, spine_set,_,_) -> 
	let d,points = 
	  TermSet.fold 
	    ( fun sigma (acc_d,acc_points) -> 
	      let d,points = calc_deriv_main all_spines sigma in

	      (* compose this spine's D matrix with the other spines' D matrices *)
	      (fun point -> 
		match (d point) with 
		  | Zero _ -> acc_d point
		  | BetaSpine (_,st,_,_) -> TermSet.union st (acc_d point)
		  | Spine _ -> 
		    failwith 
		      "why did deriv produce a Spine and not a BetaSpine?"
	      ),(U.Base.Set.union acc_points points)
	    ) spine_set ((fun _ -> TermSet.empty ), U.Base.Set.empty) in
	let points = U.Base.Set.filter_alpha points beta in
	(fun delta_gamma -> 
	  let delta = U.Base.point_lhs delta_gamma in
	  let gamma = U.Base.point_rhs delta_gamma in
	  if (U.Base.compare_complete_test beta delta) = 0
	  then 
	    make_betaspine all_spines gamma (d delta_gamma)
	  else 
	    zero ),points
      | Spine (e,_,_) -> calc_deriv_main all_spines e
	  
  let _ = default_d_matrix := (fun asp trm -> 
    let valu = ref None in 
    (fun _ -> 
      match !valu with 
	| Some e -> e
	| None -> let ret = calculate_deriv asp trm in
		  valu := Some ret; 
		  ret))
    
