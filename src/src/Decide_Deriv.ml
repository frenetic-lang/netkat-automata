
open Decide_Base
open Decide_Util

  module TermMap = Decide_Ast.TermMap
  module TermSet = Decide_Ast.TermSet
  module U = Decide_Base

  open Decide_Ast

  module rec DerivTermSet : sig
    include Set.S with type elt = DerivTerm.t 
    val default_d_matrix : (DerivTerm.t -> (unit -> ((U.Base.point -> DerivTerm.t) * U.Base.Set.t))) ref
  end = struct 
    include Set.Make(struct 
      type t = DerivTerm.t
      let compare = DerivTerm.compare
    end)
    let default_d_matrix : (DerivTerm.t -> (unit -> ((U.Base.point -> DerivTerm.t) * U.Base.Set.t))) ref = 
      ref (fun _ _ -> failwith "dummy1")
  end
  and DerivTerm : sig
    type d_matrix = ((U.Base.point -> t) * U.Base.Set.t)
    and e_matrix = U.Base.Set.t
    and d = 
      | Spine of Term.t 
      | BetaSpine of U.Base.complete_test * DerivTermSet.t 
      | Zero 
    and t = {desc : d;
	     d_matrix : unit -> d_matrix;
	     e_matrix : unit -> e_matrix; 
	     uid : int}
	     
    val to_string : t -> string 
    val compare : t -> t -> int
    val make_term : Decide_Ast.Term.t -> t
    val zero : unit -> t
    val make_spine : Term.t -> t
    val run_d : t -> d_matrix
    val run_e : t -> e_matrix
    val make_betaspine : U.Base.complete_test -> DerivTermSet.t -> t

  end = struct 

    type d_matrix = ((U.Base.point -> t) * U.Base.Set.t)
    and e_matrix = U.Base.Set.t
    and d = 
      | Spine of Term.t 
      | BetaSpine of U.Base.complete_test * DerivTermSet.t 
      | Zero 
    and t = {desc : d;
	     d_matrix : unit -> d_matrix;
	     e_matrix : unit -> e_matrix; 
	     uid : int}

    let run_e trm : U.Base.Set.t = trm.e_matrix ()
	  
    let run_d trm = trm.d_matrix ()

    let compare e1 e2 = Pervasives.compare e1.uid e2.uid  

    let default_e_matrix trm = 
      match trm.desc with
	| Spine tm ->
	  (fun _ -> Decide_Ast.Term.e_matrix tm)
	| BetaSpine (beta,ts) ->
	  let valu = ref None in 
	  (fun _ -> 
	    match !valu with 
	      | Some e -> e
	      | None -> 
		let ret = 
		  (U.Base.Set.filter_alpha
		     (DerivTermSet.fold
			(fun t -> U.Base.Set.union (run_e t) )
			ts U.Base.Set.empty)
		     beta) in 
		valu := Some ret;
		ret)
	| Zero -> 
          (fun _ -> U.Base.Set.empty) 

    let default_d_matrix = DerivTermSet.default_d_matrix
      
    let zero : unit -> DerivTerm.t = 
      let e_zero = (fun () -> U.Base.Set.empty) in
      let rec d_zero = 
	(fun () -> (fun _ -> {e_matrix = e_zero;
			      d_matrix = d_zero; 
			      uid = 0;
			      desc = Zero})
	  ,U.Base.Set.empty) in 
      (fun _ -> {e_matrix = e_zero; d_matrix = d_zero; uid = 0; desc = Zero})

	
(* TODO: hashcons please *)
    let make_spine (tm:Term.t) : DerivTerm.t = 
      (* what I wish I could write: 
	let rec ret = Spine(tm, default_e_matrix ret, !default_d_matrix ret) in ret *)
      (* what you can write is: 
	let rec ret = Spine(tm, (fun x -> default_e_matrix ret x), (fun x -> !default_d_matrix ret x)) in ret *)
      let e_matrix_option = ref None in 
      let d_matrix_option = ref None in 
      let e_matrix_backpatch = 
	(fun _ -> match !e_matrix_option with None -> failwith "1" | Some e -> e ()) in 
      let d_matrix_backpatch = 
	(fun _ -> match !d_matrix_option with None -> failwith "1" | Some d -> d ()) in      

      let rec ret = {uid = -1;
		     desc = Spine(tm); 
		     e_matrix = e_matrix_backpatch; 
		     d_matrix = d_matrix_backpatch} in
      e_matrix_option := Some (default_e_matrix ret);
      d_matrix_option := Some (!default_d_matrix ret);
      ret

    let make_betaspine beta tms = 
      let e_matrix_option = ref None in 
      let d_matrix_option = ref None in 
      let e_matrix_backpatch = 
	(fun _ -> match !e_matrix_option with None -> failwith "1" | Some e -> e ()) in 
      let d_matrix_backpatch = 
	(fun _ -> match !d_matrix_option with None -> failwith "1" | Some d -> d ()) in      

      let rec ret = {uid = -1;
		     desc = BetaSpine (beta, tms); 
		     e_matrix = e_matrix_backpatch;
		     d_matrix = d_matrix_backpatch} in

      e_matrix_option := Some (default_e_matrix ret);
      d_matrix_option := Some (!default_d_matrix ret);

      ret

    let make_term = make_spine 

    let rec to_string e = 
      match e.desc with 
	| Zero -> 
          "drop"
	| Spine t -> 
          Decide_Ast.Term.to_string t
	| BetaSpine (b,t) -> 
	  Printf.sprintf "%s;(%s)" (U.Base.complete_test_to_string b) 
	    (DerivTermSet.fold 
	       (fun x acc -> 
		 if acc = "" 
		 then (to_string x)
		 else Printf.sprintf "%s + %s" 
		   (to_string x) acc) t "")
      
  end

  open DerivTerm
    

  let laure_optimization e2 = 
    let er_E = Decide_Ast.Term.one_dup_e_matrix e2 in 
    U.Base.Set.fold 
      (fun base acc -> U.Base.Set.add (U.Base.project_lhs base) acc)
      er_E U.Base.Set.empty 
    
      
  let calc_deriv_main (e : Term.t) : ((U.Base.point -> t) * U.Base.Set.t)  = 
    let d,pts = Decide_Ast.TermPairSet.fold 
      (fun (e1,e2) (rest_d,set_of_points) -> 
	
	(* calculate e of left spine*)
	let lhs_E = Decide_Ast.Term.e_matrix e1 in 
	
	(* filter by Laure's algorithm of right spine *)
	let filtered_e =  
	  U.Base.Set.mult lhs_E (laure_optimization e2) in

	(* the D function for this pair *)
	let internal_matrix_ref point = 
	  if U.Base.Set.contains_point 
	    filtered_e point then
	    make_spine e2
	  else 
	    zero ()
	in 

	(* all points on which above function is non-Zero *)
	let more_points = 
	  U.Base.Set.union set_of_points filtered_e in
	
	(* compose this spine's D matrix with the other spines' D matrices *)
	(fun point -> 
	  let this_d = internal_matrix_ref point in 
	  match this_d.desc with 
	    | Zero -> rest_d point
	    | Spine _ -> DerivTermSet.add this_d (rest_d point)
	    | BetaSpine _ -> failwith "this can't be produced"),
	more_points)

      (Decide_Ast.Term.lrspines e)
      
      ((fun _ -> DerivTermSet.empty), U.Base.Set.empty) in
    
    (* multiply the sum of spines by Beta *)
    ((fun point -> make_betaspine (U.Base.point_rhs point) (d point)), pts)
     
  let calc_deriv_main = Decide_Ast.memoize calc_deriv_main
      
  let calculate_deriv (e:t) : ((U.Base.point -> t) * U.Base.Set.t) = 
    match e.desc with 
      | Zero -> 
	(fun _ -> zero ()), U.Base.Set.empty
      | BetaSpine (beta, spine_set) -> 
	let d,points = 
	  DerivTermSet.fold 
	    ( fun sigma (acc_d,acc_points) -> 
	      let d,points = DerivTerm.run_d sigma in
	      (* compose this spine's D matrix with the other spines' D matrices *)
	      (fun point -> 
		match (d point).desc with 
		  | Zero -> acc_d point
		  | BetaSpine (_,st) -> DerivTermSet.union st (acc_d point)
		  | Spine _ -> 
		    failwith 
		      "why did deriv produce a Spine and not a BetaSpine?"
	      ),(U.Base.Set.union acc_points points)
	    ) spine_set ((fun _ -> DerivTermSet.empty ), U.Base.Set.empty) in
	let points = U.Base.Set.filter_alpha points beta in
	(fun delta_gamma -> 
	  let delta = U.Base.point_lhs delta_gamma in
	  let gamma = U.Base.point_rhs delta_gamma in
	  if (U.Base.compare_complete_test beta delta) = 0
	  then 
	    make_betaspine gamma (d delta_gamma)
	  else 
	    zero ()),points
      | Spine e -> calc_deriv_main e
	  
  let _ = 
    DerivTermSet.default_d_matrix := 
      (fun trm -> 
        let valu = ref None in 
        (fun () -> 
          match !valu with 
	    | Some e -> 
              e
	    | None -> 
              let ret = calculate_deriv trm in
	      valu := Some ret; 
	      ret))
    
