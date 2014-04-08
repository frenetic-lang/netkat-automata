open Util
open Ast
open Term

module type UnivDescr = sig 
  type t 
  type field 
  type value 
  module FieldSet : Set.S with type elt = field
  module ValueSet : Set.S with type elt = value
  val field_compare : field -> field -> int
  val value_compare : value -> value -> int
  val all_fields : FieldSet.t
  val all_values : field -> ValueSet.t
  val field_to_string : field -> string
  val value_to_string : value -> string
end

let collection_to_string fold elt_to_string sep c = 
  fold (fun x acc -> if acc = "" else acc ^ sep ^ elt_to_string x) c ""

module Univ = functor (U : UnivDescr) -> struct 
  
  let values_to_string (vs:ValueSet.t) : string = 
    Printf.sprintf "{%s}"
      (collection_to_string U.ValueSet.fold U.value_to_string ", " s)

  module PosNeg = struct
    type t = 
        Pos of field * U.ValueSet.t
      | Neg of field * U.ValueSet.t
    let to_string = function
      | Pos (_,s) -> 
        values_to_string s
      | Neg (_,s) -> 
        "!" ^ values_to_string s 

    let any (f:field) : t = Neg(f,U.ValueSet.empty) 

    (* pre: pn1 and pn2 should be defined over the same field *)
    let compare pn1 pn2 = 
      | Pos (_,s1), Pos (_,s2) -> 
        U.ValueSet.compare s1 s2
      | Neg (_,s1), Neg (_,s2) -> 
        -1 * U.ValueSet.compare s1 s2
      | Pos (_,s1), Neg (f,s2) -> 
        U.ValueSet.compare s1 (U.ValueSet.diff (U.all_values f) s2)
      | Neg (f,s1), Pos (_,s2) -> 
        U.ValueSet.compare (U.ValueSet.diff (U.all_values f) s1) s2

    let contains (pn:t) (v:U.value) : bool = 
      match pn with 
        | Pos(_,s) -> U.ValueSet.mem v s 
        | Neg(_,s) -> not (U.ValueSet.mem v s)

    let intersect (pn1:t) (pn2:t) : t = 
      match pn1, pn2 with
        | Pos(f,s1), Pos(_,s2) -> Pos(f,U.ValueSet.inter s1 s2)
        | Neg(f,s1), Neg(_,s2) -> Neg(f,U.ValueSet.union s1 s2)
        | Pos(f,s1), Neg(_,s2) -> Pos(f,U.ValueSet.diff s1 s2)
        | Neg(f,s1), Pos(_,s2) -> Pos(f,U.ValueSet.diff s2 s1)

    let is_empty (pn:t) : bool = 
      match pn with 
        | Pos(_,s) -> U.ValueSet.is_empty s
        | Neg(f,s) -> U.ValueSet.equal (U.all_values f) s
  end
  module Base = struct

    module Map = Map.Make(U.FieldSet)      
    type atom = PosNeg.t Map.t
    type assg = ValueSet.elt Map.t
                
    let atom_to_string (a:atom) : string = 
      Map.fold (fun f pn acc -> 
        Printf.sprintf "%s%s=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (U.field_to_string f)
          (PosNeg.to_string pn))
        a ""
        
    let assg_to_string (b:assg) : string = 
      Map.fold (fun f v acc -> 
        Printf.sprintf "%s%s:=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (U.field_to_string f)
          (U.value_to_string v))
        b ""
        
    let atom_compare (a1:atom) (a2:atom) : int = 
      Map.compare PosNeg.compare 

    let assg_compare (b1:assg) (b2:assg) : int = 
      Map.compare U.value_compare

    type t = Base of atom * assg 

    let to_string (Base(a,b) : t) : string =
      Printf.sprintf "<%s;%s>" (atom_to_string a) (assg_to_string b)
        
    let compare (Base(a1,b1):t) (Base(a2,b2):t) : int =
      let cmp = atom_compare a1 a2 in
      if cmp <> 0 then cmp else assg_compare b1 b2

    let equal (x:t) (y:t) : bool = 
      compare x y = 0

    type this_t = t
    module S = Set.Make(struct
      type t = this_t
      let compare = compare
      let equal = equal
    end)

    module Set = struct
      include S
      let to_string (bs:t) : string = 
        Printf.sprintf "{%s}" 
          (S.fold (fun x s -> (if s = "" then s else s ^ ", ") ^ Base.to_string x) bs "")
    end
      
    (* Operations on Set.t *)
    exception Empty_mult

    let mult (Base(a1,b1):t) (Base(a2,b2):t) : t option = 
      try 
        U.FieldSet.fold 
          (fun f Base(a,b) -> 
            let pn1 = try Map.find f a1 with Not_found -> PosNeg.any f in 
            let pn2 = try Map.find f a2 with Not_found -> PosNeg.any f in           
            let o1 = try Some (Map.find f b1) with Not_found -> None in 
            let o2 = try Some (Map.find f b2) with Not_found -> None in 
            let pn',v' = 
              match o1,o2 with 
                | (Some v1, Some v2) when PosNeg.contains pn2 v1 -> 
                  (pn1, o2)
                | (Some v1, None) when PosNeg.contains pn2 v1 -> 
                  (pn1,o1)
                | (None, Some v2) when not (is_empty (intersect pn1 pn2)) -> 
                  (intersect pn1 pn2, o2)
                | None, None when not (is_empty (intersect pn1 pn2)) -> 
                  (intersect pn1 pn2, None) 
                | _ -> 
                  raise Empty_mult in 
            Base(Map.add f pn' a, Map.add f v' b))
          (U.all_fields) (Base(Map.empty, Map.empty)) 

    (* of_term : Ast.term -> Set.t *)
    let rec of_term (t0:Ast.term) : Set.t = 
      match t0 with 
        | One -> 
          Set.singleton (StringSetMap.empty, StringSetMap.empty)
        | Zero -> 
          Set.empty
        | Assg(x,v) -> 
          Set.singleton (StringSetMap.empty, StringSetMap.add x v StringSetMap.empty)
        | Test(x,v) -> 
          Set.singleton (StringSetMap.add x v StringSetMap.empty, StringSetMap.empty)
        | Dup -> 
          Set.empty
        | Plus ts ->
          Ast.TermSet.fold (fun t acc -> Set.union (of_term t) acc) ts Set.empty 
        | Times t -> 
          Ast.TermSet.fold (fun t acc -> mult (of_term t) acc) ts (Set.singleton (StringSetMap.empty, StringSetMap.empty))
        | Not x -> 
          assert false
        | Star x -> 
          assert false
      

    exception Intersect_fail
    let intersect (ta,pa) (tb,pb) = 
      assert (BaseElt.is_alpha ta);
      assert (BaseElt.is_alpha tb);
      assert (BaseElt.is_beta pa);
      assert (BaseElt.is_beta pb);
      try 
	let tr,pr = List.fold_left 
	  (fun (tr,pr) f -> 
	    let intersection = BaseElt.intersect_vals f ta tb in
	    if BaseElt.val_is_empty intersection then raise Intersect_fail;

	    let let_one_through px = 
	      let px_f = BaseElt.get_beta_val f px in
	      let pr_f = if (StringSetMap.val_mem px_f intersection) then px_f
		else raise Intersect_fail in
	      let tr_f = StringSetMap.val_singleton pr_f in
	      tr_f,pr_f in
	    let tr_f,pr_f = 
	      match (BaseElt.contains_key f pa),(BaseElt.contains_key f pb) with 
		| true, true -> 
		  let pa_f = BaseElt.get_beta_val f pa in
		  let pb_f = BaseElt.get_beta_val f pb in
		  if pa_f <> pb_f then raise Intersect_fail;
		  intersection, pa_f
		| false, false -> intersection, ""
		| true, false -> let_one_through pa
		| false, true -> let_one_through pb in 
	    let (tr' : R.t) = 
	      R.add_all f tr_f tr in
	    let pr' = 
	      if pr_f = "" then pr 
	      else R.add f pr_f pr in
	    tr',pr'
	  ) (R.empty,R.empty) BaseElt.all_fields in
	construct tr (R.beta_from_alpha tr pr)
	
      with Intersect_fail -> BaseElt.Empty,BaseElt.Empty
	

  end
    
  module BaseSet = struct
    include Set.Make (struct
      type t = Base.t
      let compare ((t1,p1) : t) ((t2,p2) : t) : int =
	let c = BaseElt.compare_dexter t1 t2 in
	if c = 0 then BaseElt.compare_dexter p1 p2 else c
    end)
      
    type base = Base.t

    let to_string (bs : t) : string =
      String.concat " " (fold (fun x t -> Base.to_string x :: t) bs [])
	
    let map (f : base -> base) (bs : t) : t =
      fold (fun b -> add (f b)) bs empty
	
  (* multiply BaseSets *)
    let mult (left : t) (right : t) : t =
      let f (x : base) (y : base) (r : t) : t =
	match Base.mult x y with
	  | Some z -> add z r
	  | None -> r in
      let g (x : base) (r : t) : t = fold (f x) right r in
      fold g left empty
	
    let union_list (r : t list) : t =
      List.fold_left union empty r
	
    let map (f : base -> base) (s : t) : t =
      fold (fun b r -> add (f b) r) s empty
	
    let contains alpha beta (baseSet : t) = 
      assert (match 
	  Base.intersect (alpha,beta) (alpha,beta)
	with 
	  | BaseElt.Empty,_ -> false
	  | _ -> true
      );
      assert (BaseElt.is_alpha alpha);
      assert (BaseElt.is_beta beta);
      let ret = fold 
	(fun ((t : BaseElt.t) , (p : BaseElt.t)) acc -> 
	  assert (BaseElt.is_alpha t);
	  assert (BaseElt.is_beta p);
	  match (Base.intersect (t,p) (alpha,beta)) with 
	    | BaseElt.Empty,BaseElt.Empty -> false || acc
	    | _ -> true
	)
	baseSet false in
      ret


    let to_IndexPairSet (bs : t) : IndexPairSet.t = 
      fold
	(fun e acc ->
	  let t,p = e in
	  (* add the universe to t *)
	  let t = BaseElt.get_full_stringset t in
	  let p = BaseElt.get_full_stringset p in
	  let t_exp = (IndexSet.of_StringSetMap t) in
	  let p_exp = (IndexSet.of_StringSetMap p) in
	  IndexSet.fold
	    (fun ti acc ->
	      IndexSet.fold
		(fun pi acc ->
		  IndexPairSet.add (ti,pi) acc)
		p_exp acc)
	    t_exp acc)
	bs IndexPairSet.empty 	  

    let fold_indiv (f : BaseElt.t -> BaseElt.t -> 'b -> 'b) bs acc =      
      let is = to_IndexPairSet bs in
      IndexPairSet.fold 
	(fun (l,r) acc -> 
	  let alpha,beta = Base.of_indexpair (l,r) in
	  f alpha beta acc
	) is acc

    let non_empty bs = 
      fold_indiv ( fun _ _ _ -> true) bs false
	
    let to_Normalize_B e = 
      let open Normalize in 
	  fold 
	    (fun (p,t) acc -> 
	      B.add (
		let p' = 
		  match p with 
		    | BaseElt.Alpha ssm -> ssm
		    | _ -> failwith "Bad Base pair!" in
		let t' = 
		  match t with 
		    | BaseElt.Beta (_,ssm) -> ssm 
		    | _ -> failwith "Bad Base pair!" in
		p',t')
		acc
	    ) e B.empty
	
    let equal sn tn = 
      let univ = U.univ in
      let sn = to_Normalize_B sn in
      let tn = to_Normalize_B tn in
      let sf = Normalize.fillout_ssm_pairset univ sn in
      let tf = Normalize.fillout_ssm_pairset univ tn in
      let check_leq (rhs : Normalize.B.t) ((t,p) : Normalize.ssm_pair) : bool =
	Normalize.reduced univ t;
	Normalize.leq_ssm_pairset (t,p) rhs in
      Normalize.B.for_all (Normalize.forall_atoms (check_leq tn)) sf && 
	Normalize.B.for_all (Normalize.forall_atoms (check_leq sn)) tf
	
    let to_matrix_string b = 
      let exploded_universe = (IndexSet.of_StringSetMap U.univ) in
      IndexSet.fold
	(fun l acc -> 
	  Printf.sprintf "%s\n%s"
	    acc (IndexSet.fold 
		   (fun r acc -> 
		     let alpha = BaseElt.alpha_of_index l in
		     let beta = BaseElt.beta_of_index alpha r in
		     Printf.sprintf "%s %u" 
		       acc
		       (if (contains alpha beta b) then 1 else (
			 (*Printf.printf "%s was not contained in %s"
			   (Base.to_string (alpha,beta)) (to_string b)
			 ;*)
			 0)))
		   exploded_universe ""))
	exploded_universe ""
      
  end
        
  module B = BaseSet

  let display_base (b : Base.t) =
    Printf.printf "%s\n" (Base.to_string b)
      
  let display_baseset (m : B.t) =
    print_endline (B.to_string m)
      
  let display_binding (k,l) =
    let s = String.concat "," l in
    Printf.printf "%s:{%s} " k s

  module Normalize = struct 
      
      
    type base = Base.t

(* remove assignments where preceding test is a singleton with the same value *)
    let remove_redundant_assgs (t,p) =
      let f x v = not (BaseElt.size x t = 1 && BaseElt.contains_value x v t) in
      let p = BaseElt.filter f p in (t,p)
                                 
    let remove_all_redundant_assgs : B.t -> B.t =
      B.map remove_redundant_assgs
        
(* break out a base into atoms *)
    let breakout_one (x : S.key) ((t,p) : base) : B.t =
      if BaseElt.size x t <= 1 then B.singleton (t,p)
      else let s = BaseElt.remove_all x t in
           let (t : BaseElt.t) = t in
           BaseElt.fold_key (fun v (acc : B.t) -> B.add (BaseElt.add x v s, p) acc) x t B.empty

(* this can be made more efficient using fold *)
    let breakout_more (x : S.key) (s : B.t) : B.t =
      let s = B.elements s in
      let s = List.map (breakout_one x) s in
      B.union_list s
        
    let breakout (s : B.t) : B.t =
      let k = B.elements s in
      let k = List.map (fun (_,_) -> BaseElt.all_fields) k in
      let k = remove_duplicates (List.concat k) in
      let r = List.fold_left (fun t x -> breakout_more x t) s k in
      remove_all_redundant_assgs r
        
        
(* sanity check - check that all sets in t are singletons *)
(* and all are there *)  
    let reduced (univ : S.t) (t : S.t) : unit =
      let f k = if S.contains_key k t && S.size k t = 1 then ()
        else failwith "Not reduced" in
      List.iter f (S.keys univ)
        
  end 
  let calculate_E = Normalize.normalize
end

