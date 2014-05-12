module type UnivDescr = sig 
  type field = Decide_Ast.Term.Field.t
  type value = Decide_Ast.Term.Value.t
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
	 U.ValueSet.fold Decide_Ast.Term.Value.to_string ", " vs)

  let fields_to_string (vs:U.FieldSet.t) : string = 
    Printf.sprintf "{%s}"
      (collection_to_string 
	 U.FieldSet.fold Decide_Ast.Term.Field.to_string ", " vs)

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
	(* debugging *)
      module OldMap = Map.Make(Decide_Ast.Term.Field)
      type key = Decide_Ast.Term.Field.t
      type 'a t = (('a option) Decide_Ast.Term.FieldArray.t) * ('a OldMap.t)
      let find (a : key) ((b,oldb) : 'a t) : 'a = 
	(* TODO - doesn't deal with not_found correctly. *)
	let myres = match Decide_Ast.Term.FieldArray.get b a with 
	  | Some r -> r
	  | None -> raise Not_found in 
	let theirres = OldMap.find a oldb in 
	assert (myres = theirres);
	myres


      let compare (mt,_) (cmpr : 'a -> 'a -> int) ((a,olda) : 'a t) ((b,oldb) : 'a t) : int = 
	let myres = 
	  let a,b = 
	    if a = mt 
	    then b,a
	    else a,b in
	  Decide_Ast.Term.FieldArray.fold 
	    (fun indx a' acc -> 
	      match a',(Decide_Ast.Term.FieldArray.get b indx) with 
		| Some a', Some b' -> 
		  if acc = 0 
		  then cmpr a' b'
		  else acc
		| a',b' -> 
		  Pervasives.compare a' b' ) a 0 in 
	let theirres = OldMap.compare cmpr olda oldb in 
	assert ( (if (myres = 0) then (0 = theirres) else true)
		 && (if (theirres = 0) then (0 = myres) else true));
	myres
	    
      let fold (f : key -> 'a -> 'b -> 'b) ((st,oldst) : 'a t) (acc : 'b) : 'b = 
	let myres = 
	  Decide_Ast.Term.FieldArray.fold 
	    (fun indx b acc -> 
	      match b with 
		| Some e -> (f indx e acc)
		| None -> acc) st acc in 
	let theirres = OldMap.fold f oldst acc in 
	assert (myres = theirres);
	myres


      let to_string (m,oldm) elt_to_string = 
	Printf.sprintf "Mine: [%s]\nTheirs: [%s]\n\n"
	  (Decide_Ast.Term.FieldArray.fold 
	     (fun a b acc -> 
	       match b with 
		 | None -> acc 
		 | Some b -> 
		   Printf.sprintf "%s : %s\n%s" 
		     (Decide_Ast.Term.Field.to_string a)
		     (elt_to_string b) acc) m "")
	  (OldMap.fold 
	     (fun a b -> 
	       Printf.sprintf "%s : %s\n%s" 
		 (Decide_Ast.Term.Field.to_string a)
		 (elt_to_string b)) oldm "")


      let compare _ prntr (cmpr : 'a -> 'a -> int) ((a,olda) : 'a t) ((b,oldb) : 'a t) : int = 
	let myres = 
	  U.FieldSet.fold 
	    (fun fld acc -> 
	      if acc = 0
	      then 
		match Decide_Ast.Term.FieldArray.get a fld,(Decide_Ast.Term.FieldArray.get b fld) with 
		  | Some a', Some b' -> 
		    cmpr a' b'
		  | a',b' -> 
		    Pervasives.compare a' b'
	      else acc) U.all_fields 0 in 
	let theirres = OldMap.compare cmpr olda oldb in 
	assert ( 
	  (if (myres = 0) 
	   then (if 0 <> theirres then Printf.eprintf "%s vs\n %s" (to_string (a,olda) prntr) (to_string (b,oldb) prntr);
		 0 = theirres) else true)
	  && (if (theirres = 0) then (0 = myres) else true));
	myres


      let add (a : key) (b : 'a) ((arr,oldarr) : 'a t) : 'a t = 
	let myres = 
	  let newarr = Decide_Ast.Term.FieldArray.copy arr in 
	  Decide_Ast.Term.FieldArray.set newarr a (Some b);
	  newarr in 
	let theirres = 
	  OldMap.add a b oldarr in 
	myres,theirres

      let empty : 'a t = (Decide_Ast.Term.FieldArray.make None, OldMap.empty)

    end

    type atom = PosNeg.t Map.t
    type assg = U.ValueSet.elt Map.t
    let (atom_empty : atom) = Map.empty
    let (assg_empty : assg) = Decide_Ast.Term.FieldArray.make None, Map.OldMap.empty

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
          (Decide_Ast.Term.Field.to_string f)
          pnstr)
        U.all_fields ""
        
    let assg_to_string (b:assg) : string = 
      U.FieldSet.fold (fun f acc -> 
	let vstr = 
	  try Decide_Ast.Term.Value.to_string (Map.find f b)
	  with Not_found -> "_"
	in
        Printf.sprintf "%s%s:=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (Decide_Ast.Term.Field.to_string f)
          vstr)
        U.all_fields ""
        
    let atom_compare (a1:atom) (a2:atom) : int = 
      Map.compare atom_empty PosNeg.to_string PosNeg.compare a1 a2

    let assg_compare (b1:assg) (b2:assg) : int = 
      Map.compare assg_empty Decide_Ast.Term.Value.to_string Decide_Ast.Term.Value.compare b1 b2

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
	  try Decide_Ast.Term.Value.to_string (Map.find f b)
	  with Not_found -> "_"
	in
        Printf.sprintf "%s%s=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (Decide_Ast.Term.Field.to_string f)
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
      let x = Map.fold 
	(fun f v acc -> Map.add f (PosNeg.Pos(f,U.ValueSet.singleton v)) acc)
	x Map.empty in
      Base(x, y)

    
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



    let completetest_to_term_test x : Decide_Ast.InitialTerm.t = 
      Decide_Ast.InitialTerm.Times
	(U.FieldSet.fold 
	   (fun field acc -> 
	     let v = try Map.find field x with Not_found -> 
	       failwith "Point doesn't match the spec."  in
	     (Decide_Ast.InitialTerm.Test(field, v))::acc)
	   U.all_fields [])

    exception Empty_filter

    let filter_alpha (Base(a1,b1):t) (a2: complete_test) : t option =
      try
	Some (U.FieldSet.fold
		(fun field (Base(tests, assgs)) ->
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
		  let new_assg = (try Some (Map.find field b1) 
		    with Not_found -> None) in
		  match new_assg with
		    | None -> Base(Map.add field new_test tests, assgs)
		    | Some new_assg ->
		      Base(Map.add field new_test tests,
			   Map.add field new_assg assgs))
		U.all_fields (Base(atom_empty, assg_empty)))
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
                | (Some v1, Some v2) when PosNeg.contains pn2 v1 -> 
                  (pn1, o2)
                | (Some v1, None) when PosNeg.contains pn2 v1 -> 
                  (pn1, o1)
                | (None, Some v2) when not 
		    (PosNeg.is_empty 
		       (PosNeg.intersect pn1 pn2)) -> 
                  (PosNeg.intersect pn1 pn2, o2)
                | None, None when not (PosNeg.is_empty 
					 (PosNeg.intersect pn1 pn2)) -> 
                  (PosNeg.intersect pn1 pn2, None) 
                | _ -> 
                  raise Empty_mult in 
            Base(Map.add field pn' a, 
                 match o' with
                   | None -> b
                   | Some v' -> Map.add field v' b))
          (U.all_fields) (Base(atom_empty, assg_empty)))
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
	    ) U.all_fields [Point(assg_empty, assg_empty)]
      in
      let pts = (extract_points b) in
      List.fold_right f pts acc

    let project_lhs (Base(a,b)) = 
      Base(a,assg_empty)
	
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
	let module ValHash = Decide_Ast.Term.ValueArray in
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

	let phase1 = fold 
	  (fun b  acc -> 
	    let assg = match b with Base(_,assg) -> assg in 
	    List.map
	      (fun (field,valset,valhash,others) -> 
		try 
		  let a = Map.find field assg in 
		  field,U.ValueSet.add a valset,incr_add a b valhash, others
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
	      Printf.printf "%u%%\n" ((100 * cardinal cnds)  / (cardinal left));
	      fold (fun lhs acc -> 
		match (mult lhs rhs) with 
		  | Some r -> Printf.printf "sucesscount\n"; add r acc
		  | None -> Printf.printf "failedcount\n"; acc
	      ) cnds acc) phase2 empty in 
	if Decide_Util.debug_mode (* TODO : make this debug mode only *)
	then (assert (equal (old_mult left right) phase3); phase3)
	else phase3

      let mult = old_mult


      let biggest_cardinal = ref 0 

    (* of_term : Ast.term -> Set.t *)
    (* this calculates the E matrix *)
      let of_term t0 = 
	Printf.printf "entered ofterm!\n%!";
	let t0 = Decide_Ast.zero_dups t0 in 
	  (* may only normalize dup-free terms *)
	let t0 = Decide_Ast.deMorgan t0 in
	let of_term : (Decide_Ast.term -> t) ref  = 
	  ref (fun _ -> failwith "dummy") in 
	of_term := Decide_Ast.memoize (fun (t0:Decide_Ast.term)  -> 
	  (* to negate a test x=v, we allow x to have any value except v *)
	  let negate (x : U.field) (v : U.value) =
	    singleton(Base(Map.add x 
			     (PosNeg.Neg(x,U.ValueSet.singleton v)) 
			     atom_empty,assg_empty))
	  in
	  let open Decide_Ast.Term in 
	      match t0 with 
		| One _ -> 
		  singleton (Base(atom_empty, assg_empty))
		| Zero _ -> 
		  empty
		| Assg(_,field,v) -> 
		  singleton (Base(atom_empty, Map.add field v assg_empty))
		| Test(_,field,v) ->  
		  singleton 
		    (Base(Map.add field 
			    (PosNeg.Pos (field,U.ValueSet.singleton v)) 
			    atom_empty, assg_empty))
		| Dup _ -> 
		  empty
		| Plus (_,ts) ->
		  Decide_Ast.TermSet.fold 
		    (fun t acc -> union (!of_term t) acc) ts empty 
		| Times (_,tl) -> 
		  List.fold_right (fun t acc ->  mult (!of_term t) acc) tl
		    (singleton (Base (atom_empty, assg_empty)))
		| Not (_,x) -> begin
		  match x with
		    | Zero _ -> singleton (Base(atom_empty,assg_empty))
		    | One _ -> empty
		    | Test (_,x,v) -> negate x v
		    | _ -> failwith "De Morgan law should have been applied"
		end
		| Star (_,x) ->
		  let s = !of_term x in
		  let s1 = add (Base(atom_empty, assg_empty)) s in
		  let rec f s r  =
		    if equal s r then s
		    else f (mult s s) s in
		  f (mult s1 s1) s1 );
	let ret = !of_term t0 in 
	Printf.printf "%u\n\n" (cardinal ret);
	let cardinal = cardinal ret in 
	if cardinal > !biggest_cardinal 
	then biggest_cardinal := cardinal;
	ret 

    let of_term = Decide_Ast.memoize of_term

    let filter_alpha bs (beta : complete_test) = 
      fold
	(fun b acc -> 
	  match (filter_alpha b beta) with 
	    | None -> acc
	    | Some r -> add r acc)
	bs empty


    let print_debugging_info _ = 
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
      


    end (* Base.Set *)	    

  end (* Base *)
end (* Univ *)
