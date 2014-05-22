module type UnivDescr = sig 
  type field = Decide_Util.Field.t
  type value = Decide_Util.Value.t
  module FieldSet : Set.S with type elt = field
  module ValueSet : Set.S with type elt = value
  val all_fields : FieldSet.t
  val all_values : field -> ValueSet.t
end

let collection_to_string fold elt_to_string sep c =
  fold (fun x acc ->
    if acc = "" 
    then acc ^ elt_to_string x 
    else acc ^ sep ^ elt_to_string x) c ""

module Univ = functor (U : UnivDescr) -> struct 

    		  
  let values_to_string (vs:U.ValueSet.t) : string = 
    Printf.sprintf "{%s}"
      (collection_to_string 
	 U.ValueSet.fold Decide_Util.Value.to_string ", " vs)

  let fields_to_string (vs:U.FieldSet.t) : string = 
    Printf.sprintf "{%s}"
      (collection_to_string 
	 U.FieldSet.fold Decide_Util.Field.to_string ", " vs)

  module PosNeg = struct
    type t = 
        Pos of U.field * U.ValueSet.t
      | Neg of U.field * U.ValueSet.t
    let to_string = function
      | Pos (_,s) -> 
        values_to_string s
      | Neg (f,s) -> 
        values_to_string (U.ValueSet.diff (U.all_values f) s)

    let empty (f:U.field) : t = Pos(f, U.ValueSet.empty)
      
    let singleton (f: U.field) (e : U.ValueSet.elt) : t = 
      Pos(f,U.ValueSet.singleton e)

    let any (f:U.field) : t = Neg(f, U.ValueSet.empty) 

    (* pre: pn1 and pn2 should be defined over the same field *)
    let compare pn1 pn2 = 
      match pn1, pn2 with 
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
        | Pos(_,s) -> 
          U.ValueSet.mem v s 
        | Neg(_,s) -> 
          not (U.ValueSet.mem v s)
          
    let intersect (pn1:t) (pn2:t) : t = 
      match pn1, pn2 with
        | Pos(f,s1), Pos(_,s2) -> 
          Pos(f,U.ValueSet.inter s1 s2)
        | Neg(f,s1), Neg(_,s2) -> 
          Neg(f,U.ValueSet.union s1 s2)
        | Pos(f,s1), Neg(_,s2) -> 
          Pos(f,U.ValueSet.diff s1 s2)
        | Neg(f,s1), Pos(_,s2) -> 
          Pos(f,U.ValueSet.diff s2 s1)

    let is_empty (pn:t) : bool = 
      match pn with 
        | Pos(_,s) -> 
          U.ValueSet.is_empty s
        | Neg(f,s) -> 
          U.ValueSet.equal (U.all_values f) s

    let elements (pn : t) : U.ValueSet.t = 
      match pn with 
	| Pos (_,elts) -> elts
	| Neg (f,elts) -> 
	  (U.ValueSet.diff (U.all_values f) elts)
	      
  end (* PosNeg *)
  module Base = struct

    module Map = struct 
      type key = Decide_Util.Field.t
      type 'a t = (('a option) Decide_Util.FieldArray.t) 
      let find (a : key) (b : 'a t) : 'a = 
	match Decide_Util.FieldArray.get b a with 
	  | Some r -> r
	  | None -> raise Not_found
	    
      let get a b = Decide_Util.FieldArray.get b a
    
      let fold (f : key -> 'a -> 'b -> 'b) (st : 'a t) (acc : 'b) : 'b = 
	  Decide_Util.FieldArray.fold 
	    (fun indx b acc -> 
	      match b with 
		| Some e -> (f indx e acc)
		| None -> acc) st acc 

      let to_string m elt_to_string = 
	Printf.sprintf "[%s]\n"
	  (Decide_Util.FieldArray.fold 
	     (fun a b acc -> 
	       match b with 
		 | None -> acc 
		 | Some b -> 
		   Printf.sprintf "%s : %s\n%s" 
		     (Decide_Util.Field.to_string a)
		     (elt_to_string b) acc) m "")

      let compare (cmpr : 'a -> 'a -> int) (a : 'a t) (b : 'a t) : int = 
	U.FieldSet.fold 
	  (fun fld acc -> 
	    if acc = 0
	    then 
	      match Decide_Util.FieldArray.get a fld,(Decide_Util.FieldArray.get b fld) with 
		| Some a', Some b' -> 
		  cmpr a' b'
		| a',b' -> 
		  Pervasives.compare a' b'
	    else acc) U.all_fields 0 
      
      let add (a : key) (b : 'a) (arr : 'a t) : 'a t = 
	let newarr = Decide_Util.FieldArray.copy arr in 
	Decide_Util.FieldArray.set newarr a (Some b);
	newarr 

      let init f = Decide_Util.FieldArray.init 
	(fun a -> Some (f a))

      let init' = Decide_Util.FieldArray.init

      let singleton k v = 
	init' (fun k' -> if k = k' then Some v else None)

      let empty : unit -> 'a t = (fun _ -> (Decide_Util.FieldArray.make None))

    end

    type atom = PosNeg.t Map.t
    type assg = U.ValueSet.elt Map.t
    let (atom_empty : unit -> atom) = Map.empty
    let (assg_empty : unit -> assg) = Map.empty

    let atom_to_string (a:atom) : string = 
      U.FieldSet.fold (fun f acc -> 
	let pnstr = 
	  try 
	    PosNeg.to_string (Map.find f a)
	  with Not_found -> 
	    PosNeg.to_string (PosNeg.any f)
	in
        Printf.sprintf "%s%s=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (Decide_Util.Field.to_string f)
          pnstr)
        U.all_fields ""
        
    let assg_to_string (b:assg) : string = 
      U.FieldSet.fold (fun f acc -> 
	let vstr = 
	  try Decide_Util.Value.to_string (Map.find f b)
	  with Not_found -> "_"
	in
        Printf.sprintf "%s%s:=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (Decide_Util.Field.to_string f)
          vstr)
        U.all_fields ""
        
    let atom_compare (a1:atom) (a2:atom) : int = 
      Map.compare PosNeg.compare a1 a2

    let assg_compare (b1:assg) (b2:assg) : int = 
      Map.compare Decide_Util.Value.compare b1 b2

    type t = Base of atom * assg 
    (* must be a Pos * completely-filled-in thing*)
    type point = Point of assg * assg

    let compare_point (Point(al,ar)) (Point (bl,br)) = 
      match (assg_compare al bl) with 
	| 0 -> (assg_compare ar br)
	| k -> k
	
    type complete_test = assg

    let compare_complete_test = assg_compare 
    let complete_test_to_string b =       
      U.FieldSet.fold (fun f acc -> 
	let vstr = 
	  try Decide_Util.Value.to_string (Map.find f b)
	  with Not_found -> "_"
	in
        Printf.sprintf "%s%s=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (Decide_Util.Field.to_string f)
          vstr)
        U.all_fields ""

    let point_to_string (Point(x,y)) = 
      Printf.sprintf "<%s,%s>" (complete_test_to_string x) (assg_to_string y)

    let point_rhs (Point(_,r)) = r
    let point_lhs (Point(l,_)) = l

    let to_string (Base(a,b) : t) : string =
      Printf.sprintf "<%s;%s>" (atom_to_string a) (assg_to_string b)
        
    let compare (Base(a1,b1):t) (Base(a2,b2):t) : int =
      let cmp = atom_compare a1 a2 in
      if cmp <> 0 then cmp else assg_compare b1 b2

    let compar = compare

    let equal (x:t) (y:t) : bool = 
      compare x y = 0

    type this_t = t
    module S = Set.Make(struct
      type t = this_t
      let compare = compare
      let equal = equal
    end)
      
    exception Empty_mult

    let base_of_point (Point(x,y) : point) : t = 
      let x = 
	Map.init 
	  (fun f -> 
	    match Map.get f x with 
	      | Some v -> (PosNeg.Pos(f,U.ValueSet.singleton v))
	      | None -> failwith "point doesn't match the spec") in 
      Base(x,y)

    
    let contains_point (Point(x,y) : point) (Base(a,b) : t) : bool = 
      U.FieldSet.fold 
	(fun field acc -> 
	  let x = try Map.find field x with Not_found -> 
	    failwith "Point doesn't match the spec." in
	  let y = try Map.find field y with Not_found -> 
	    failwith "Point doesn't match the spec." in
	  let a = try Map.find field a with Not_found -> 
	    (PosNeg.any field) in
	  PosNeg.contains a x && 
	    (try y = (Map.find field b)
	     with Not_found -> y = x)
	  && acc
	) U.all_fields true



    let completetest_to_term_test x : (unit Decide_Ast.term) list = 
      (U.FieldSet.fold 
	 (fun field acc -> 
	   let v = try Map.find field x with Not_found -> 
	     failwith "Point doesn't match the spec."  in
	   (Decide_Ast.make_test (field, v))::acc)
	 U.all_fields [])
	

    exception Empty_filter

    let filter_alpha (Base(a1,b1):t) (a2: complete_test) : t option =
      try
	let test_Fun = 
	  (fun field -> 
	       let test_1 =
		 try Map.find field a1 with Not_found -> 
		   PosNeg.any field in
	       let test_2 =
		 try Map.find field a2 with Not_found -> 
		   failwith "complete test has missing field!" in
	       let new_test = 
		 if PosNeg.contains test_1 test_2 
		 then PosNeg.singleton field test_2
		 else PosNeg.empty field
	       in
	       if PosNeg.is_empty new_test
	       then raise Empty_filter;
	       new_test) in
	let assg_fun = 
	  (fun field -> 
	    let new_assg = (try Some (Map.find field b1) 
	      with Not_found -> None) in
	    new_assg) in
	Some  (Base (Map.init test_Fun, Map.init' assg_fun))
      with Empty_filter -> None
	     
    let mult (Base(a1,b1):t) (Base(a2,b2):t) : t option = 
      try 
        Some (U.FieldSet.fold 
          (fun field (Base(a,b)) ->
            let pn1 = try Map.find field a1 with Not_found -> 
	      PosNeg.any field in 
            let pn2 = try Map.find field a2 with Not_found -> 
	      PosNeg.any field in           
            let o1 = try Some (Map.find field b1) with Not_found -> None in 
            let o2 = try Some (Map.find field b2) with Not_found -> None in 
            let pn',o' = 
              match o1,o2 with 
                | (Some v1, Some v2) -> (pn1, o2)
                | (Some v1, None) -> (pn1, o1)
		| (None, _) -> 
		  let inter = (PosNeg.intersect pn1 pn2) in
		    if (PosNeg.is_empty inter) then raise Empty_mult;
		    inter,o2 in
            Base(Map.add field pn' a, 
                 match o' with
                   | None -> b
                   | Some v' -> Map.add field v' b))
          (U.all_fields) (Base((atom_empty ()), (assg_empty ()))))
      with Empty_mult -> 
        None

    let fold_points (f : (point -> 'a -> 'a)) (Base(a,b) : t) (acc : 'a) 
	: 'a =
      let extract_points bse : point list = 
	  U.FieldSet.fold
	    (fun field partial_list -> 
	      let a = PosNeg.elements (try Map.find field a 
		with Not_found -> 
		  (PosNeg.any field)) in
	      List.fold_right 
		(fun (Point(x,y)) (acc : point list) -> 
		  try 
		    let b = Map.find field b in
		    U.ValueSet.fold (fun v acc -> 
		      (Point(Map.add field v x, Map.add field b y)) :: acc
		    ) a acc
		  with Not_found ->		    
		    U.ValueSet.fold (fun v acc -> 
		      (Point(Map.add field v x, Map.add field v y)) :: acc
		    ) a acc
		) partial_list []
	    ) U.all_fields [Point((assg_empty ()), (assg_empty ()))]
      in
      let pts = (extract_points b) in
      List.fold_right f pts acc

    let project_lhs (Base(a,b)) = 
      Base(a,(assg_empty ()))

    let univ_base = (Base((atom_empty ()), (assg_empty ())))

    let of_assg field v = Base((atom_empty ()), Map.singleton field v )

    let of_test field v = 
      Base(Map.singleton field (PosNeg.Pos (field,U.ValueSet.singleton v)), assg_empty ())

    let of_neg_test x v = 
      Base(
	Map.singleton x (PosNeg.Neg(x,U.ValueSet.singleton v)), 
	assg_empty ())
	
    module Set = struct
      include S

      let to_string (bs:t) : string = 
        Printf.sprintf "{%s}" 
          (S.fold 
	     (fun x s -> 
	       (if s = "" then s else s ^ ", ") ^ to_string x) bs "")
	  

      let failed_Count = Decide_Util.failed_Count
      let success_count = Decide_Util.success_count
      (* TODO: a more efficient multiplication would be nice.*)
      let old_mult (left : t) (right : t) : t =
	let f x y (r : t) : t =
          match mult x y with
            | Some z -> success_count := !success_count + 1; assert (!success_count > 0); add z r
            | None -> failed_Count := !failed_Count + 1; assert (!failed_Count > 0); r in
	let g x  (r : t) : t = fold (f x) right r in
	fold g left empty

      let contains_point (st : t) (pt : point) : bool = 
	fold (fun e acc -> (contains_point pt e) || acc) st false
	
      (* we're dealing with duplicates right now by 
	 rememering and skipping points.  
	 It would be better to invent a normal form 
	 which guaranteed no duplicates.
      *)

      let wasted_cycles = ref 0
      let total_cycles = ref 1
	
      let fold_points (f : (point -> 'a -> 'a)) (st : t) (acc : 'a) : 'a =
	let seen_points = ref S.empty in
	fold (fun base acc -> 
	  fold_points (fun elt acc -> 
	    total_cycles := !total_cycles + 1;
	    let belt = base_of_point elt in 
	    if S.mem belt (!seen_points)
	    then (wasted_cycles := !wasted_cycles + 1; acc)
	    else (seen_points := S.add belt (!seen_points); 
		  (f elt acc))) 
	    base acc
	) st acc


      let shallow_equal a b = 
	if cardinal a = cardinal b 
	then List.fold_left2
	  (fun (acc : bool) (a : elt) (b : elt) -> 
	    acc && (0 = compar a b)
	  ) true (elements a) (elements b) 
	else false

      let compare a b = failwith "can't do it"
	    

      let equal (a : t) (b : t) = 
	fold_points (fun pt acc -> contains_point b pt && acc) a true
	&& fold_points (fun pt acc -> contains_point a pt && acc) b true


      let union (a : t) (b : t) : t = 
	union a b


      let mult (left : t)  (right : t) : t = 
	let module ValHash = Decide_Util.ValueArray in
	let extract_test (f : U.field) (Base(atom,_)) = 
	  try Map.find f atom 
	  with Not_found -> PosNeg.any f in 
	let pn_intersect (a : U.ValueSet.t) (b : PosNeg.t) : U.ValueSet.t = 
	  match a, b with
            | s1, PosNeg.Pos(_,s2) -> 
              U.ValueSet.inter s1 s2
            | s1, PosNeg.Neg(_,s2) -> 
              U.ValueSet.diff s1 s2 in
	let intersect_all lst = 
	  List.fold_left 
	    (fun acc e -> inter e acc) (List.hd lst) (List.tl lst) in 
	let incr_add k v hash = 
	  ValHash.set hash k (add v (ValHash.get hash k));
	  hash in
	let incr_add_all ks v hash = 
	  U.ValueSet.iter 
	    (fun k -> 
	      ValHash.set hash k (add v (ValHash.get hash k))) ks;
	  hash in

	let phase1 = fold 
	  (fun b  acc -> 
	    let Base(test,assg) = b in
	    List.map
	      (fun (field,valset,valhash,others) -> 
		try 
		  let a = Map.find field assg in 
		  field,U.ValueSet.add a valset,incr_add a b valhash, others
		with Not_found -> 
		  try 
		    let a = Map.find field test in 
		    match a with 
		      | PosNeg.Pos(_,a) -> field,U.ValueSet.union a valset,incr_add_all a b valhash, others
		      | _ -> raise Not_found
		  with Not_found -> 
		    field,valset,valhash,add b others) acc)
	  left (U.FieldSet.fold 
		  (fun f acc -> 
		    (f,U.ValueSet.empty,ValHash.make empty,empty)::acc) 
		  U.all_fields []) in

	let phase2 = 
	  fold 
	    (fun b acc -> 
	      let to_intersect = 
		List.map
		  (fun (f,vs,vh,bs) -> 
		    let i = pn_intersect vs (extract_test f b) in 
		    union 
		      (U.ValueSet.fold 
			 (fun a -> union (ValHash.get vh a)) i empty) 
		      bs) phase1 in 
	      (b,intersect_all to_intersect)::acc) right [] in 

	let phase3 = 
	  List.fold_right
	    (fun (rhs,cnds) acc -> 
	      fold (fun lhs acc -> 
		match (mult lhs rhs) with 
		  | Some r -> success_count := !success_count + 1; assert (!success_count > 0); add r acc
		  | None -> failed_Count := !failed_Count + 1; assert (!failed_Count > 0); acc
	      ) cnds acc) phase2 empty in 

	if Decide_Util.debug_mode (* TODO : make this debug mode only *)
	then (assert (equal (old_mult left right) phase3); phase3)
	else phase3

      let mult = old_mult

      let biggest_cardinal = ref 0 


    let filter_alpha bs (beta : complete_test) = 
      fold
	(fun b acc -> 
	  match (filter_alpha b beta) with 
	    | None -> acc
	    | Some r -> add r acc)
	bs empty


    let print_debugging_info _ = ()
(*
      Printf.printf "Biggest uid for AST: %u\n" (Decide_Ast.Term.int_of_uid (Decide_Ast.Term.largest_uid ()));
      Printf.printf "Failed count : %u\n" !failed_Count;
      Printf.printf "success count: %u\n" !success_count;
      Printf.printf "percent failed : %u\n" ((100 * !failed_Count) / (!failed_Count + !success_count));
      Printf.printf "Total iterations of fold_points: %d\n" !total_cycles;
      Printf.printf "Total wasted cycles in fold_points: %d\n" 
	!wasted_cycles;
      (Printf.printf "Percent wasted cycles: %d\n" 
	 ((!wasted_cycles * 100) / !total_cycles));
      Printf.printf "size of biggest BaseSet: %u" (!biggest_cardinal);
      Printf.printf "size of the universe (for reference): %u" 
	(U.FieldSet.fold (fun fld acc -> 
	  U.ValueSet.cardinal (U.all_values fld) * acc) U.all_fields 1) ;
      *)      


    end (* Base.Set *)	    

  end (* Base *)
end (* Univ *)
