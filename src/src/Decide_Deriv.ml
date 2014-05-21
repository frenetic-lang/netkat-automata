
open Decide_Base
open Decide_Util
open Decide_Spines

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
  module Cached = Decide_Ast_Cache.Ast(UDesc)
  module Spines = Decide_Spines.Spines(UDesc)
  module TermMap = Map.Make(Cached.Term)
  module TermSet = Decide_Ast.TermSet
  type spines_map = Cached.cached_info Decide_Ast.term_set TermMap.t


  open Cached

  module rec DerivTerm : sig
    type e_matrix = | E_Matrix of (unit -> U.Base.Set.t)
    and d_matrix = | D_Matrix of (unit -> 
				((U.Base.point -> t) * U.Base.Set.t))
	
    and t = 
      | Spine of Term.t (* actual term *) * 
	(* for speedy Base.Set calculation *) e_matrix ref * 
      (* for speedy Deriv calculation*) d_matrix ref
      | BetaSpine of U.Base.complete_test * Cached.cached_info TermSet.t * 
	(* for speedy Base.Set calculation *) e_matrix ref * 
	(* for speedy Deriv calculation*) d_matrix ref
      | Zero of e_matrix ref * d_matrix ref

    val to_string : t -> string 
    val compare : t -> t -> int
    val make_term : unit Decide_Ast.term -> t
    val make_spine : spines_map -> Term.t -> t
    val make_zero : unit -> t
    val make_betaspine : spines_map -> 
      U.Base.complete_test -> Cached.cached_info TermSet.t -> t
    val default_d_matrix : (spines_map -> t -> d_matrix) ref
  end = struct 
    type e_matrix = | E_Matrix of (unit -> U.Base.Set.t)
    and d_matrix = | D_Matrix of (unit -> 
				((U.Base.point -> t) * U.Base.Set.t))
	
    and t = 
      | Spine of Term.t (* actual term *) * 
	(* for speedy Base.Set calculation *) e_matrix ref * 
      (* for speedy Deriv calculation*) d_matrix ref
      | BetaSpine of U.Base.complete_test * Cached.cached_info TermSet.t * 
	(* for speedy Base.Set calculation *) e_matrix ref * 
	(* for speedy Deriv calculation*) d_matrix ref
      | Zero of e_matrix ref * d_matrix ref


    let derivterm_to_term e = 
      let f = function 
	| Spine (d,_,_) -> d
	| BetaSpine (b,tms,_,_) -> 
	  Cached.times (List.flatten [List.map Cached.of_term (U.Base.completetest_to_term_test b); [(Cached.plus tms)]])
	| Zero _ -> Cached.zero
	  in
      (f e)
	

    let compare e1 e2 = 
      let rec compare runthatassert e1 e2 = 
	let my_compare = 
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
	      ) in 
	if debug_mode
	then let their_compare = 
	       Pervasives.compare (derivterm_to_term e1) (derivterm_to_term e2) in 
	     assert (if my_compare = 0 then their_compare = 0 else true); 
	     if runthatassert
	     then assert (compare false e2 e1 = my_compare * -1);
	     my_compare
      else my_compare in
      compare true e1 e2
	  

    let default_e_matrix trm =
      let new_matrix = 
      (E_Matrix (fun _ -> match trm with 
	| Spine (tm,em,_) -> 
	  let ret = (Cached.get_cache tm).e_matrix in 
	  em := (E_Matrix (fun _ -> ret)); ret
	| BetaSpine (beta,ts,em,_) -> 
	  let ret = U.Base.Set.filter_alpha 
	    (TermSet.fold 
	       (fun t -> U.Base.Set.union (Cached.get_cache t).e_matrix )
	       ts U.Base.Set.empty) 
	    beta in 
	  em := (E_Matrix (fun _ -> ret));
	  ret
	| Zero(em,_) -> 
	  let ret = U.Base.Set.empty in
	  em := (E_Matrix (fun _ -> ret));
	  ret)) in 
      if debug_mode 
      then 
	let old_matrix = 
	  (E_Matrix (fun _ -> match trm with 
	    | Spine (_,em,_)
	    | BetaSpine (_,_,em,_)
	    | Zero (em,_)
	      -> 
	      let ret = (Cached.get_cache (derivterm_to_term trm)).e_matrix in 
	      em := (E_Matrix (fun _ -> ret)); ret
	   )) in 
	assert (U.Base.Set.equal
	    (match old_matrix with E_Matrix f -> (f ())) 
	    (match new_matrix with E_Matrix f -> (f ())));
	new_matrix
      else new_matrix


    let default_d_matrix = ref (fun _ _ -> failwith "dummy1")
      
    let default_e_zero = ref (E_Matrix (fun _ -> U.Base.Set.empty))
      
    let default_d_zero = 
      ref (D_Matrix(fun _ -> (fun _ -> failwith "dummy2"),U.Base.Set.empty))
      
    let _ = default_d_zero := (D_Matrix (fun _ -> 
      (fun _ -> Zero(default_e_zero,default_d_zero)),
      U.Base.Set.empty))
      
    let make_spine (all_spines : spines_map) e2 = 
      let em_fun = ref (E_Matrix (fun _ -> failwith "dummy3")) in 
      let d_fun = ref (D_Matrix (fun _ -> failwith "dummy4")) in 
      let ret = (Spine (e2,em_fun ,d_fun)) in 
      em_fun := (default_e_matrix ret);
      d_fun := ((!default_d_matrix) all_spines ret);
      ret

    let make_zero _ = 
      (Zero (default_e_zero ,default_d_zero))
	
    let make_betaspine (all_spines : spines_map) beta tm = 
      let em_fun = ref (E_Matrix (fun _ -> failwith "dummy5")) in 
      let d_fun = ref (D_Matrix (fun _ -> failwith "dummy6")) in 
      let ret = BetaSpine (beta, tm,em_fun,d_fun) in
      em_fun := default_e_matrix ret;
      d_fun := !default_d_matrix all_spines ret;
      ret

	
	
    let make_term = (fun (e : unit Decide_Ast.term) -> 
      make_spine 
	(Spines.TermMap.fold 
	   (fun tm ts acc -> 
	     TermMap.add (Cached.of_term tm) 
	       (TermSet.map (fun e -> Cached.of_term e) ts)
	       acc)
	   (Spines.allLRspines e) TermMap.empty ) (Cached.of_term e))

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
    | (Spine(_,e,_) | Zero(e,_) | BetaSpine(_,_,e,_)) -> 
      (match (!e) with 
	| E_Matrix e -> e ())
	
  let run_d trm = match trm with 
    | (Spine(_,_,d) | Zero(_,d) | BetaSpine(_,_,_,d)) -> 
      (match (!d) with 
	| D_Matrix d -> d ())
	
      
    
      
  let calc_deriv_main all_spines (e : Term.t) : 
      ((U.Base.point -> t) * U.Base.Set.t)  = 
    let d,pts = TermSet.fold 
      (fun spine_pair (acc,set_of_points) -> 
	(* pull out elements of spine pair*)
	let e1,e2 = match spine_pair with 
	  | Decide_Ast.Term.Times (_,[lspine;rspine],_) -> lspine,rspine
	  | _ -> failwith "Dexter LIES" in
	
	(* calculate e of left spine*)
	let corresponding_E = (Cached.get_cache e1).e_matrix in
	let er_E = (Cached.get_cache e2).one_dup_e_matrix in
	let er_E' = U.Base.Set.fold 
	  (fun base acc -> U.Base.Set.add (U.Base.project_lhs base) acc)
	  er_E U.Base.Set.empty in
	let e_where_intersection_is_present =  
	  U.Base.Set.mult corresponding_E er_E' in
	let internal_matrix_ref point = 
	  if U.Base.Set.contains_point 
	    e_where_intersection_is_present point then
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
      (TermMap.find e all_spines) 
      ((fun _ -> TermSet.empty()), U.Base.Set.empty) in
    (fun point -> 
      make_betaspine all_spines (U.Base.point_rhs point) (d point)
    ), pts

  let calc_deriv_main = Decide_Ast.memoize_on_arg2 calc_deriv_main
      
  let calculate_deriv all_spines (e : t) : 
      ((U.Base.point -> t) * U.Base.Set.t) = 
    match e with 
      | (Zero _ | Spine (Decide_Ast.Term.Zero _,_,_)) -> 
	(fun _ -> (make_zero ())), 
	U.Base.Set.empty
      | BetaSpine (beta, spine_set,_,_) -> 
	let d,points = 
	  TermSet.fold 
	    ( fun sigma (acc_d,acc_points) -> 
	      let d,points = calc_deriv_main all_spines sigma in
	      (fun point -> 
		match (d point) with 
		  | Zero _ -> acc_d point
		  | Spine _ -> 
		    failwith 
		      "why did deriv produce a Spine and not a BetaSpine?"
		  | BetaSpine (_,s,_,_) -> TermSet.union s (acc_d point)
	      ),(U.Base.Set.union acc_points points)
	    ) spine_set ((fun _ -> TermSet.empty () ), U.Base.Set.empty) in
	let points = U.Base.Set.filter_alpha points beta in
	(fun delta_gamma -> 
	  let delta = U.Base.point_lhs delta_gamma in
	  let gamma = U.Base.point_rhs delta_gamma in
	  if (U.Base.compare_complete_test beta delta) = 0
	  then 
	    (
	      make_betaspine all_spines gamma (d delta_gamma))
	  else (
		make_zero ())),points
      | Spine (e,_,_) -> 
	calc_deriv_main all_spines e
	  
  let _ = default_d_matrix := (fun asp trm -> 
    D_Matrix (fun _ -> 
      match trm with 
	| (Spine (_,_,dm) | BetaSpine (_,_,_,dm) | Zero(_,dm)) -> 
	  let ret = calculate_deriv asp trm in
	  dm := D_Matrix((fun _ -> ret));
	  ret)) 
    
end
