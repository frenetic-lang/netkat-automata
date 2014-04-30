
open Ast
open Ast.Term
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

(* collect subterms *)
let rec subterms (e : term) : TermSet.t =
  match e with
  | (Assg _ | Test _ | Not _ | Zero | One | Dup) -> TermSet.singleton e
  | Plus ts ->
    let f x t = TermSet.union t (subterms x) in
    TermSet.fold f ts (TermSet.singleton e)
  | Times l ->
    let u = List.map subterms l in
    let f ts x = TermSet.union ts x in
	  List.fold_left f (TermSet.singleton e) u
  | Star d ->
    let s = subterms d in
    TermSet.add e s

let spines_of_subterms (e : term) : TermSet.t =
  let u = subterms e in
  TermSet.bind u rspines
  
(* sanity check -- all spines of subterms of spines of subterms *)
(* must already be spines of subterms *)
(* the above is not a typo *)
let ss_sanity (e : term) : bool =
  let ss = spines_of_subterms e in
  TermSet.for_all (fun x -> TermSet.subset (spines_of_subterms x) ss) ss

module TermMap = Map.Make(struct 
  type t = Term.term
  let compare = Pervasives.compare (* prolly want our own compare here *)
end
)

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
  let module U = Univ(UnivDescr) in 
  (* calculate all spines as the first thing in the main algorithm
     and pass them in here *)

  let calc_deriv_main all_spines e = 
    TermSet.fold 
      (fun spine_pair (acc,set_of_points) -> 
	  (* pull out elements of spine pair*)
	let e1,e2 = match spine_pair with 
	  | Times [lspine;rspine] -> lspine,rspine
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
	    mul_terms (U.Base.test_of_point point) e2
	  else 
            Zero in 
	let more_points = 
	  U.Base.Set.union set_of_points e_where_intersection_is_present in
	
	(fun point -> add_terms (internal_matrix_ref point) (acc point)),
	more_points)
      (Hashtbl.find all_spines e) ((fun _ -> Zero), U.Base.Set.empty) in
  let calc_deriv_main = Util.memoize_on_arg2 calc_deriv_main in
  

  let rec calculate_deriv all_spines (e:Ast.term) =
    (* TODO(jnf,ljt,mpm): fill in the type *)
    try 
      calc_deriv_main all_spines e
    with Not_found -> 
      begin 
        if (Ast.contains_dups e) then 
          calculate_deriv (allLRspines e) e
        else 
          ((fun _ -> Zero),U.Base.Set.empty)
      end
  in
  
  let calculate_deriv = Util.memoize_on_arg2 calculate_deriv in

  let module WorkList = WorkList(struct 
    type t = (Term.term * Term.term) 
    let compare = Pervasives.compare
  end) in

  let spines_t1 = allLRspines t1 in
  let spines_t2 = allLRspines t2 in

  let get_state,update_state,print_states = 
    (* Dot.init (fun a -> not (U.Base.Set.is_empty a)) *)
    (fun _ _ _ _ -> true,true,1,1), (fun _ _ _ _ _ -> ()), (fun _ -> ())
  in

  let rec main_loop work_list = 
    if WorkList.is_empty work_list
    then 
      (print_states (); true)
    else
      let q1,q2 = WorkList.hd work_list in
      let rest_work_list = WorkList.tl work_list in
      let q1_E = U.Base.Set.of_term q1 in
      let q2_E = U.Base.Set.of_term q2 in
      if not (U.Base.Set.equal q1_E q2_E)
      then false
      else
	
	let (dot_bundle : Dot.t) = get_state q1 q2 q1_E q2_E in
	let q1_matrix,q1_points = calculate_deriv spines_t1 q1 in 
	let q2_matrix,q2_points = calculate_deriv spines_t2 q2 in 
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
	      (U.Base.Set.of_term q1')
	      (U.Base.Set.of_term q2');
	    WorkList.add (q1',q2')
	      expanded_work_list
	  )
	  (U.Base.Set.union q1_points q2_points) rest_work_list in
	main_loop work_list in
  main_loop (WorkList.singleton (t1,t2))


