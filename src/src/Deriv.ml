
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
  let univ = List.fold_left (fun u x -> StringSetMap.add x "<other>" u) univ (StringSetMap.keys univ) in
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
  end in 
  let module U = Univ(UnivDescr) in 

  (* calculate all spines as the first thing in the main algorithm
     and pass them in here *)
  let rec calculate_deriv all_spines (e:Ast.term) =
    (* TODO(jnf,ljt,mpm): fill in the type *)
    try 
      Printf.printf "asked for the deriv of %s \n" (Ast.term_to_string e);
      TermSet.fold 
	(fun spine_pair (acc,set_of_points) -> 
	  (* pull out elements of spine pair*)
	  let e1,e2 = match spine_pair with 
	    | Times [lspine;rspine] -> lspine,rspine
	    | _ -> failwith "Dexter LIES" in

	(* calculate e of left spine*)
	let corresponding_E = U.Base.Set.of_term e1 in
	  
	(* use previous intersection to determine non-zero elements of D(e) *)
	Printf.printf "left spine in deriv: %s \n" (Ast.term_to_string e1);
        (* TODO(jnf): the commented out line below is attempting to
           further shrink the set of base pairs that we have to
           consider while calculating the rest of the derivative by
           calculating E(e2), that is, the right spine, and then
           multiplying by E(e1), that is, the left spine. There is
           some unknown bug that is tickled by enabling this, so
           it's currently disabled. *)
	(* TODO(mpm): The bug was actually a somewhat interesting algorithmic error, 
	   and has been fixed in the legacy code.  It requires some interface re-structuring,
	   and has thus been delayed until after the current re-factor is stable.
	*)
	let e_where_intersection_is_present = corresponding_E  in
	let internal_matrix_ref point = 
	  if U.Base.Set.contains_point e_where_intersection_is_present point then
	    mul_terms (U.Base.test_of_point point) e2
	  else 
            Zero in 
	let more_points = 
	  U.Base.Set.union set_of_points e_where_intersection_is_present in

	(fun point -> add_terms (internal_matrix_ref point) (acc point)),
	more_points)
	(Hashtbl.find all_spines e) ((fun _ -> Zero), U.Base.Set.empty)
    with Not_found -> 
      begin 
        Printf.printf "couldn't find requested expression in spines.\n";
        if (Ast.contains_dups e) then 
          calculate_deriv (allLRspines e) e
        else 
          ((fun _ -> Zero),U.Base.Set.empty)
      end
  in

  let module WorkList = WorkList(struct 
    type t = (Term.term * Term.term) 
    let compare = Pervasives.compare
  end) in

  let spines_t1 = allLRspines t1 in
  let spines_t2 = allLRspines t2 in

  let f1 = open_out "/tmp/fsm1.dot" in
  let f2 = open_out "/tmp/fsm2.dot" in
  let cell1,cell2 = ref 0, ref 0 in
  let states1, states2 = ref [], ref [] in
  let edges1, edges2 = ref [], ref [] in

  let get_state1 t f =
    let s = Ast.term_to_string t in
    try fst (List.assoc s !states1)
    with Not_found ->
      incr cell1;
      states1 := (s,(!cell1,f))::!states1;
      !cell1 in
  let get_state2 t f =
    let s = Ast.term_to_string t in
    try fst (List.assoc s !states2)
    with Not_found ->
      incr cell2;
      states2 := (s,(!cell2,f))::!states2;
      !cell2 in
  let add_edge1 q1 q2 =
    edges1 := (q1,q2)::!edges1 in
  let add_edge2 q1 q2 =
    edges2 := (q1,q2)::!edges2 in
				 
  let rec main_loop work_list = 
    Printf.printf "Iterating the work list! \n";
    if WorkList.is_empty work_list
    then 
      begin
	(* no primes *)
	Printf.fprintf f1 "digraph fsm {\n";
	Printf.fprintf f1 "rankdir=LR;\n";
	Printf.fprintf f1 "node [shape=circle]; ";
	List.iter (fun (_,(x,_)) -> Printf.fprintf f1 " q%d" x)
	  (List.filter (fun (_,(_,f)) -> not f) !states1);
	Printf.fprintf f1 "\n";
	Printf.fprintf f1 "node [shape=doublecircle]; ";
	List.iter (fun (_,(x,_)) -> Printf.fprintf f1 " q%d" x)
	  (List.filter (fun (_,(_,f)) -> f) !states1);
	Printf.fprintf f1 "\n";
	List.iter (fun (x,y) -> Printf.fprintf f1 "q%d -> q%d;\n" x y) !edges1;
	Printf.fprintf f1 "}\n";
	(* primes *)
	Printf.fprintf f2 "digraph fsm {\n";
	Printf.fprintf f2 "rankdir=LR;\n";
	Printf.fprintf f2 "node [shape=circle]; ";
	List.iter (fun (_,(x,_)) -> Printf.fprintf f2 " q%d" x)
	  (List.filter (fun (_,(_,f)) -> not f) !states2);
	Printf.fprintf f2 "\n";
	Printf.fprintf f2 "node [shape=doublecircle]; ";
	List.iter (fun (_,(x,_)) -> Printf.fprintf f2 " q%d" x)
	  (List.filter (fun (_,(_,f)) -> f) !states2);
	Printf.fprintf f2 "\n";
	List.iter (fun (x,y) -> Printf.fprintf f2 "q%d -> q%d;\n" x y) !edges2;
	Printf.fprintf f2 "}\n";
	close_out f1;
	close_out f2;
	ignore (Sys.command "dot -Tpdf -o /home/research/research/fsm1.pdf /tmp/fsm1.dot");
	ignore (Sys.command "dot -Tpdf -o /home/research/research/fsm2.pdf /tmp/fsm2.dot");
      true
      end
    else
      let q1,q2 = WorkList.hd work_list in
      let rest_work_list = WorkList.tl work_list in
      let q1_E = U.Base.Set.of_term q1 in
      let q2_E = U.Base.Set.of_term q2 in
      Printf.printf "The universe: %s\n" 
	(StringSetMap.to_string univ "%s={%s}" (fun x -> x));
      Printf.printf "q1: %s\n" (Ast.term_to_string q1);
      Printf.printf "q2: %s\n" (Ast.term_to_string q2);
      Printf.printf "E of q1: %s\n" (U.Base.Set.to_string q1_E);
      Printf.printf "E of q2: %s\n" (U.Base.Set.to_string q2_E);
      (*
	TODO(mpm): Re-write this pretty-printer at some point.
	Printf.printf "E of q1 in matrix form:\n%s\n" (BaseSet.to_matrix_string q1_E);
      Printf.printf "E of q2 in matrix form:\n%s\n" (BaseSet.to_matrix_string q2_E);*)

      (*TODO(mpm):  all of this assumes a normal form.  If the bases inside the set are empty,
	or there are structurally-different representations of equal things, this will all break.
      *)
      if not (U.Base.Set.equal q1_E q2_E)
      then false
      else
	let f1 = not (U.Base.Set.is_empty (q1_E)) in
	let f2 = not (U.Base.Set.is_empty (q2_E)) in
	let z1 = get_state1 q1 f1 in
	let z2 = get_state2 q2 f2 in
	let _ = Printf.printf "q%d %b\n" z1 f1 in
	let _ = Printf.printf "q%d %b\n" z2 f2 in
	let q1_matrix,q1_points = calculate_deriv spines_t1 q1 in 
	let q2_matrix,q2_points = calculate_deriv spines_t2 q2 in 
	let work_list = U.Base.Set.fold_points
	  (fun pt expanded_work_list -> 
	    let q1' = q1_matrix pt in
	    let q2' = q2_matrix pt in
	    Printf.printf "q1': %s\n" (Ast.term_to_string q1');
	    Printf.printf "q2': %s\n" (Ast.term_to_string q2');
	    let q1'_E = U.Base.Set.of_term q1' in
	    let q2'_E = U.Base.Set.of_term q2' in
	    let f1' = not (U.Base.Set.is_empty (q1'_E)) in
	    let f2' = not (U.Base.Set.is_empty (q2'_E)) in
	    let z1' = get_state1 q1' f1' in
	    let z2' = get_state2 q2' f2' in
	    let _= add_edge1 z1 z1' in
	    let _ = add_edge2 z2 z2' in
	    WorkList.add (q1',q2' )
	      expanded_work_list
	  )
	  (U.Base.Set.union q1_points q2_points) rest_work_list in
	main_loop work_list in
  main_loop (WorkList.singleton (t1,t2))


