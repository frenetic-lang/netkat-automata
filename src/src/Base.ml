open Util
open Ast
open Term

module S = StringSetMap

(* A base is a pair (t,p) of StringSetMaps, where t is a set of tests      *)
(* of the form x in A for a variable x and set of values A and each x      *)
(* occurs at most once in t, and p is a sequence of assignments x := n,    *)
(* where each x occurs at most once in p. The values in p are singletons.  *)

module type UnivDescr = sig 
  val univ : StringSetMap.t 
end

module Univ = functor (U : UnivDescr) -> struct 

  module M = Map.Make (String)
  module Index = struct 
    type t = string M.t
    let compare = M.compare String.compare
    let add = M.add
    let empty = M.empty
    let fold = M.fold
    
    let to_test i = 
      Term.Plus
	(fold
	   (fun f v acc -> 
	     TermSet.add (Term.Test (f,v)) acc
	   )
	   i TermSet.empty)

    let to_string i = 
      Printf.sprintf "(%s)" (fold (Printf.sprintf "%s=%s;%s") i "")
  end 
	
  module IndexSet = struct 
    include Set.Make (struct 
      type t = Index.t
      let compare = Pervasives.compare
    end)
      
    let of_StringSetMap t =
      let module IListSet = Set.Make(struct
	type t = (string * string) list
	let compare = Pervasives.compare
      end ) in

      let stringsetmap_to_indexset ssm =
	List.fold_left
	  (fun (set_of_lists : IListSet.t ) (field : string) ->
	    
	    let per_val_algo (valu : string)
		(new_set_of_lists : IListSet.t) :
		IListSet.t =
	      IListSet.union
		(IListSet.fold
		   (fun (ilist : IListSet.elt)
		     ( new_set_of_lists : IListSet.t) ->
		       let ilist' = ((field,valu)::ilist) in
		       IListSet.add ilist' new_set_of_lists
		   )
		   set_of_lists IListSet.empty)
		new_set_of_lists in
	    let sol : IListSet.t = (StringSetMap.fold_key
				      per_val_algo
				      field ssm IListSet.empty) in
	    sol
	  )
	  (IListSet.singleton []) (StringSetMap.keys ssm) in
      
      let tuple_univ_repr = stringsetmap_to_indexset t in
      let ret = IListSet.fold
	(fun complete_test acc ->
	  add
	    (List.fold_left
	       (fun (acc : Index.t) ((field : string),(value : string)) ->
		 Index.add field value acc
	       ) Index.empty complete_test)
	    acc
	)
	tuple_univ_repr empty in
      ret
	
      
  end
	
  module IndexPairSet = struct 
    include Set.Make (struct
      type t = Index.t * Index.t
      let compare = Pervasives.compare 
    end)

    let all_pairs t_exp p_exp = 
      IndexSet.fold 
	(fun ti acc -> 
	  IndexSet.fold 
	    (fun pi acc -> 
	      add (ti,pi) acc)
	    p_exp acc)
	t_exp empty 

  end
  
  module BaseElt = struct 

    exception Universe_Exception
      
    (* TODO: stronger compare? *)
    let compare = Pervasives.compare

 
    let all_fields = StringSetMap.keys U.univ
      
    type t = 
      | Alpha of StringSetMap.t
      | Beta of t * StringSetMap.t
      | Empty 

    let is_alpha = function 
      | Alpha _ -> true
      | _ -> false

    let is_beta = function
      | Beta _ -> true
      | _ -> false

    let compare_dexter a b = 
      let ssmc = StringSetMap.compare in
      match a,b with 
	| Alpha ssm1, Beta (_,ssm2) -> ssmc ssm1 ssm2
	| Alpha ssm1, Alpha ssm2 -> ssmc ssm1 ssm2
	| Beta (_,ssm1), Beta (_,ssm2) -> ssmc ssm1 ssm2
	| Beta (_,ssm1), Alpha ssm2 -> ssmc ssm1 ssm2
	| (Empty,_ | _,Empty) -> failwith "don't compare empties."
	  
    type field = StringSetMap.key
	
    let field_as_string f = f

    let rec find_all f b1 = 
      if StringSetMap.contains_key f U.univ then
	match b1 with 
	  | Alpha ssm -> 
	    if StringSetMap.contains_key f ssm
	    then StringSetMap.find_all f ssm
	    else StringSetMap.find_all f U.univ
	  | Beta (up,ssm) -> 
	    if StringSetMap.contains_key f ssm
	    then StringSetMap.find_all f ssm
	    else find_all f up
	  | Empty -> StringSetMap.val_empty
      else failwith "trying to find a key not in the universe! bad!"
	
  (* removes redundancies between Alpha and universe 
     and between A and B
  *)
    let rec normalize r = 
      match r with 
	| Alpha ssm -> 
	  Alpha (List.fold_left
		   (fun acc f -> 
		     let r_contents = StringSetMap.find_all f ssm in
		     let univ_contents = StringSetMap.find_all f U.univ in 
		     if StringSetMap.val_equal r_contents univ_contents
		     then acc
		     else StringSetMap.add_all f r_contents acc)
		   StringSetMap.empty all_fields)
	| Beta (up,ssm) -> 
	  let up = normalize up in
	  let ssm = StringSetMap.fold 
	    (fun k v acc -> if v = "" then acc else StringSetMap.add k v acc)
	    ssm StringSetMap.empty in
	  Beta (up, (List.fold_left 
		       (fun acc f -> 
			 let r_contents = StringSetMap.find_all f ssm in
			 let up_contents = find_all f up in
			 (* does not contain the empty string *)
			 assert (StringSetMap.fold_key 
				   (fun e acc -> 
				     e <> "" && acc
				   )
				   f ssm true);
			 if (StringSetMap.val_equal r_contents up_contents)
			 then acc
			 else StringSetMap.add_all f r_contents acc)
		       StringSetMap.empty all_fields))
	| Empty -> r
	  
    let beta_from_alpha up r = 
      match r with 
	| Alpha ssm -> normalize (Beta (up,ssm))
	| _ -> failwith "you gave me a non-alpha..."
	  

  (* this can only make an alpha *)
    exception Empty_Intersection
    let intersect b1 b2 = 
      match b1,b2 with 
	| Alpha _, Alpha _ -> 
	  (try 
	    Alpha (List.fold_left
		     (fun acc f ->
		       let b1_f = find_all f b1 in
		       let b2_f = find_all f b2 in
		       let inter = StringSetMap.val_inter b1_f b2_f in
		       if StringSetMap.val_is_empty inter
		       then raise Empty_Intersection
		       else StringSetMap.add_all f inter acc
		     ) StringSetMap.empty all_fields)
	  with Empty_Intersection -> Empty)
	| _ -> failwith "It doesn't make sense to intersect non-alphas at the BaseElt level" 

    let contains_value f v r = 
      let vals = find_all f r in
      StringSetMap.val_mem v vals

    let get_beta_val f b = 
      match b with 
	| Beta (up,b) ->  
	  StringSetMap.fold_key (fun e _ -> e) f b ""
	| _ -> failwith "it's not a beta.  fix it."

    let get_full_stringset a = 
      List.fold_left
	(fun acc f -> 
	  StringSetMap.add_all f (find_all f a) acc)
	StringSetMap.empty all_fields
	
    let to_string (t : t) : string =
      let ssm_to_string ssm op = StringSetMap.to_string ssm op (fun x -> x) in
      match t with 
	| Alpha _ -> ssm_to_string (get_full_stringset t) "%s={%s}"
	| Beta (_,b) -> ssm_to_string b "%s=%s"
	| _ -> failwith "I just hate Empty."

    let rec sanity_check a = 
      match a with 
	| Alpha ssm -> List.for_all (fun f -> StringSetMap.contains_key f U.univ) (StringSetMap.keys ssm)
	| Beta (up,ssm) -> 
	  (List.for_all (fun f -> StringSetMap.single_mapping f ssm) all_fields)
	  && sanity_check up
	| Empty -> true
	  
  (* matches the intuition we had when we wrote the multiplication algorithm
     for alphas, it includes inheriting from the universe.
     for betas, it doesn't.
  *)
    let contains_key k t = 
      match t with 
	| Alpha ssm -> failwith "the answer is always YES here.."
	| Beta (_,ssm) -> 
	  if StringSetMap.contains_key k ssm 
	  then (assert ((StringSetMap.size k ssm) = 1); 
		true)
	  else false
	| Empty -> failwith "just don't do operations on empty."
	  
    let contains_value k v t = 
      match t with 
	| Alpha ssm -> 
	  if StringSetMap.contains_key k ssm
	  then StringSetMap.contains_value k v ssm
	  else true
	| _ -> failwith "contains_value only makes sense for alpha"
	  
    let add_all f v t = 
      match t with 
	| Alpha ssm -> Alpha (StringSetMap.add_all f v ssm)
	| _ -> failwith "I'm only defining add_all on alphas."
	  
	  
    let intersect_vals f a b = 
      let a_f = find_all f a in
      let b_f = find_all f b in 
      StringSetMap.val_inter a_f b_f
	
    let empty = Alpha (StringSetMap.empty)
      
    let val_is_empty = StringSetMap.val_is_empty
	
    let construct_pair a b : t * t = 
      match a,b with 
	| Alpha _, Beta _ -> 
	  let a = normalize a in
	  let b = normalize b in
	  assert (sanity_check a);
	  assert (sanity_check b);
	  a,b
	| _ -> failwith "I just want inheritance, is that so much to ask for?"


    let size f a = 
      StringSetMap.val_size (find_all f a)

    let add k v t = 
      match t with 
	| Alpha a -> Alpha (StringSetMap.add k v a)
	| Beta (up,a) -> Beta (up, StringSetMap.add k v a)
	| Empty -> failwith "shouldn't add to this kind of Empty.  Add to BaseElt.empty instead"	

    let beta_of_index a i = 
      beta_from_alpha a (Index.fold add i empty)

    let alpha_of_index i = 
      Index.fold add i empty


    (* NOTE: all the below is present to support dexter-code.  
       so it just flat-forwards to the underlying maps.*)


    let filter f t = 
      match t with 
	| (Alpha a ) -> Alpha (StringSetMap.filter f a)
	| Beta (up,a) -> Beta (up, StringSetMap.filter f a)
	| Empty -> failwith "Dexter code should NOT get empty"

    let remove_all f t = 
      match t with 
	| Alpha a -> Alpha (StringSetMap.remove_all f a)
	| Beta (up,a) -> Beta (up,StringSetMap.remove_all f a)
	| Empty -> failwith "Dexter code should NOT get empty"

    let fold_key (f : StringSetMap.elt -> 'b -> 'b)  (k : StringSetMap.key) (t : t) (acc : 'b) : 'b =
      match t with 
	| Alpha a -> (StringSetMap.fold_key f k a acc)
	| Beta (up,a) -> StringSetMap.fold_key f k a acc
	| Empty -> failwith "Dexter code should NOT get empty"

      	  
    let dexter_union a b = 
      match a with 
	| Alpha a -> Alpha (StringSetMap.union a b)
	| Beta (up,a) -> Beta(up,StringSetMap.union a b)
	| Empty -> failwith "Dexter code should NOT get empty"	
	  
  end

  module Base = struct 
    module R = BaseElt
    type t = R.t * R.t
	
    let construct = R.construct_pair
      
    let to_string ((t,p) : t) : string =
      Printf.sprintf "[%s,%s]" (R.to_string t) (R.to_string p )

    let of_stringsetmaps a b = 
      let alpha = BaseElt.Alpha a in
      let beta = BaseElt.Beta (alpha,b) in
      construct alpha beta

    let of_indexpair (a,b) = 
      let alpha = BaseElt.alpha_of_index a in
      construct alpha (BaseElt.beta_of_index alpha b)
	
    (* multiply two bases *)
    exception Mult_fail
    let mult ((ta,pa) : t) ((tb,pb) : t) : t option =
      
      try Some (
	let l,r = 
	  List.fold_left 
	    (fun acc f -> 
	      let (tr : R.t),pr = acc in
	      let tr_f = 
		if R.contains_key f pa 
		then 
		  (
		    if (not (R.contains_value f (R.get_beta_val f pa) tb))
		    then (
		      raise Mult_fail
		    );
		    R.find_all f ta)
		else 
		  let intersection = R.intersect_vals f ta tb in
		  if R.val_is_empty intersection
		  then (
		    raise Mult_fail)
		  else intersection in
	      let pr_f = 
		if R.contains_key f pb
		then R.get_beta_val f pb
		else R.get_beta_val f pa in
	      let (tr' : R.t) = 
		R.add_all f tr_f tr in
	      let pr' = 
		R.add f pr_f pr in
	      tr',pr'
	    ) (R.empty,R.empty) R.all_fields in
	construct l (R.beta_from_alpha l r)
      )
      with Mult_fail -> 
	None
	
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
        
    (* parameter univ is the universe *)
    let normalize = 
      let hash = Hashtbl.create 0 in
      (* do we only need to take the universe here as a sanity check? *)
      let rec normalize  t = 
        let normalize (t : term) : B.t =
          (* to negate a test x=v, we allow x to have any value except v *)
          let negate x v =
            (* sanity check *)
	    if not (S.contains_value x v U.univ)
	    then failwith (Printf.sprintf "Unknown variable/value %s/%s" x v);
	    let s = S.fold_key (S.add x) x U.univ S.empty in
	    let negated_ssm = (S.remove x v s) in
	    Printf.printf "normalize negate: %s\n" (StringSetMap.to_string negated_ssm "%s={%s}" (fun x -> x));
	    B.singleton (Base.of_stringsetmaps negated_ssm S.empty) in
          let t = Ast.zero_dups t in (* may only normalize dup-free terms *)
          let t = Ast.deMorgan t in
          match t with 
	    | Assg (x,v) ->
	  (* I thought that the LHS needed to be fully specified.
	     Is there an assumption that "missing" elements in h1
	     simply inherit from the universe?
	  *)
	      let h1 = S.empty in
	      let h2 = S.add x v (S.empty) in
	      B.singleton (Base.of_stringsetmaps h1 h2)
	    | Test (x,v) ->
	  (*
	    are these complete tests and assignments or partial ones?
	    They look like partial ones.
	    Same question as above.
	  *)
	      let h1 = S.add x v (S.empty) in
	      let h2 = S.empty in
	      B.singleton (Base.of_stringsetmaps h1 h2)
	    | Dup -> failwith "Cannot normalize a term with dups"
	    | Plus x ->
	      let s = List.map (normalize ) (TermSet.elements x) in
	      B.union_list s
	    | Times x ->
	      let s = List.map (normalize ) x in
	      let mult_res = List.fold_left 
	        (fun acc e -> 
	          B.mult acc e) 
	        (B.singleton (Base.of_stringsetmaps S.empty S.empty)) s in
	      mult_res
	    | Not x -> begin
	      match x with
	        | Zero -> B.singleton (Base.of_stringsetmaps S.empty S.empty)
	        | One -> B.empty
	        | Test (x,v) -> negate x v
	        | _ -> failwith "De Morgan law should have been applied"
	    end
	    | Star x ->
	      let s = normalize  x in
	      let s1 = B.add (Base.of_stringsetmaps S.empty S.empty) s in
	      let rec f (s : B.t) (r : B.t) : B.t =
                if B.equal s r then s
                else f (B.mult s s) s in
	      f (B.mult s1 s1) s1
	    | Zero -> B.empty
	    | One -> B.singleton (Base.of_stringsetmaps S.empty S.empty) in
        try Hashtbl.find hash t
        with Not_found -> 
          let ret = normalize t in
          Hashtbl.add hash t ret;
          ret in
      normalize
  end 
  let calculate_E = Normalize.normalize
end

