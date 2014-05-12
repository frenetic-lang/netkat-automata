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

    module Map = Decide_Ast.Term.FieldArray
    let _ = assert (U.FieldSet.cardinal U.all_fields > 0) 
    let empty_map = 
      let ret = 
	Map.make (PosNeg.any (U.FieldSet.choose U.all_fields)) in
      U.FieldSet.iter
	(fun f -> 
	  Map.set ret f (PosNeg.any f)) U.all_fields;
      ret
    let make_map _ = Map.copy empty_map
    type atom = PosNeg.t Decide_Ast.Term.FieldArray.t
    type assg = (U.ValueSet.elt option) Decide_Ast.Term.FieldArray.t

    let atom_to_string (a:atom) : string = 
      U.FieldSet.fold (fun f acc -> 
	let pnstr = 
	  PosNeg.to_string (Map.get a f)
	in
        Printf.sprintf "%s%s=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (Decide_Ast.Term.Field.to_string f)
          pnstr)
        U.all_fields ""
        
    let assg_to_string (b:assg) : string = 
      U.FieldSet.fold (fun f acc -> 
	let vstr = 
	  match Map.get b f with 
	    | Some e -> Decide_Ast.Term.Value.to_string e
	    | None -> "_"
	in
        Printf.sprintf "%s%s:=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (Decide_Ast.Term.Field.to_string f)
          vstr)
        U.all_fields ""
        
    let atom_compare (a1:atom) (a2:atom) : int = 
      Map.fold
	(fun indx e1 acc -> 
	  if acc = 0
	  then PosNeg.compare e1 (Map.get a2 indx)
	  else acc) a1 0

    let assg_compare (b1:assg) (b2:assg) : int = 
      Map.fold
	(fun indx e1 acc -> 
	  if acc = 0
	  then match e1,Map.get b2 indx with 
	    | Some e1,Some e2 -> Decide_Ast.Term.Value.compare e1 e2
	    | None ,  Some _ -> 1
	    | Some _, None -> -1
	    | None, None -> 0
	else acc) b1 0

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
	  match Map.get b f with 
	    | Some e -> Decide_Ast.Term.Value.to_string e
	    | None -> "_"
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
	(fun f v acc -> 
	  let v = match v with 
	    | Some v -> v 
	    | None -> failwith "point didn't match the spec" in 
	  Map.set acc f (PosNeg.Pos(f,U.ValueSet.singleton v)); acc)
	x (make_map ()) in
      Base(x, y)

    
    let contains_point (Point(x,y) : point) (Base(a,b) : t) : bool = 
      U.FieldSet.fold 
	(fun field acc -> 
	  let x = match Map.get x field with 
	    | Some x -> x
	    | None -> failwith "Point doesn't match the spec." in
	  let y = match Map.get y field with 
	    | Some y -> y
	    | None -> failwith "Point doesn't match the spec." in
	  let a = Map.get a field in
	  PosNeg.contains a x && 
	    (match Map.get b field with 
	      | Some y' -> y = y'
	      | None -> y = x)
	  && acc
	) U.all_fields true



    let completetest_to_term_test x : Decide_Ast.InitialTerm.t = 
      Decide_Ast.InitialTerm.Times
	(U.FieldSet.fold 
	   (fun field acc -> 
	     let v = match Map.get x field with 
	       | Some x -> x
	       | None -> failwith "Point doesn't match the spec."  in
	     (Decide_Ast.InitialTerm.Test(field, v))::acc)
	   U.all_fields [])

    exception Empty_filter

    let filter_alpha (Base(a1,b1):t) (a2: complete_test) : t option =
      try
	Some (U.FieldSet.fold
		(fun field (Base(tests, assgs)) ->
		  let test_1 = Map.get a1 field in 
		  let test_2 = match Map.get a2 field with 
		    | Some x -> x
		    | None -> failwith "complete test has missing field!" in
		  let new_test = 
		    if PosNeg.contains test_1 test_2 
		    then PosNeg.singleton field test_2
		    else PosNeg.empty field
		  in
		  if PosNeg.is_empty new_test
		  then raise Empty_filter;
		  let new_assg = Map.get b1 field in
		  match new_assg with
		    | None -> Base((Map.set tests field new_test; tests) , assgs)
		    | Some new_assg ->
		      Base((Map.set tests field new_test; tests),
			   (Map.set assgs field (Some new_assg); assgs)))
		U.all_fields (Base(make_map(),Map.make None)))
      with Empty_filter -> None

    let mult (Base(a1,b1):t) (Base(a2,b2):t) : t option = 
      try 
        Some (U.FieldSet.fold 
          (fun field (Base(a,b)) ->
            let pn1 = Map.get a1 field in 
            let pn2 = Map.get a2 field in   
            let o1 = Map.get b1 field in 
            let o2 = Map.get b2 field in 
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
            Base((Map.set a field pn'; a), 
                 (match o' with
                   | None -> b
                   | Some v' -> Map.set b field (Some v'); b)))
          (U.all_fields) (Base(make_map(), Map.make None)))
      with Empty_mult -> 
        None

    let fold_points (f : (point -> 'a -> 'a)) (Base(a,b) : t) (acc : 'a) 
	: 'a =
      let extract_points bse : point list = 
	  U.FieldSet.fold
	    (fun field partial_list -> 
	      let a = PosNeg.elements (Map.get a field)  in 
	      List.fold_right 
		(fun (Point(x,y)) (acc : point list) -> 
		  match Map.get b field with 
		    | Some b -> 
		      U.ValueSet.fold (fun v acc -> 
			(Point((Map.set x field (Some v); x), (Map.set y field (Some b); y))) :: acc
		      ) a acc
		    | None ->		    
		      U.ValueSet.fold (fun v acc -> 
			(Point((Map.set x field (Some v); x), (Map.set y field (Some v); y))) :: acc
		      ) a acc
		) partial_list []
	    ) U.all_fields [Point(Map.make None, Map.make None)]
      in
      let pts = (extract_points b) in
      List.fold_right f pts acc

    let project_lhs (Base(a,b)) = 
      Base(a,Map.make None)
	
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
	  Map.get atom f in
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
		match Map.get assg field with 
		  | Some a -> 
		    field,U.ValueSet.add a valset,incr_add a b valhash, others
		  | None -> 
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
	    singleton(Base(
	      (let ret = make_map () in 
	       Map.set ret x (PosNeg.Neg(x,U.ValueSet.singleton v)); ret),Map.make None))
	  in
	  let open Decide_Ast.Term in 
	      match t0 with 
		| One _ -> 
		  singleton (Base(make_map (), Map.make None))
		| Zero _ -> 
		  empty
		| Assg(_,field,v) -> 
		  singleton (Base(make_map (),
				  ( let ret = 
				      Map.make None in 
				    Map.set ret field (Some v); ret)))
		| Test(_,field,v) ->  
		  singleton 
		    (Base((let ret = make_map () in 
			   Map.set ret field (PosNeg.Pos (field,U.ValueSet.singleton v));
			   ret), Map.make None))
		| Dup _ -> 
		  empty
		| Plus (_,ts) ->
		  Decide_Ast.TermSet.fold 
		    (fun t acc -> union (!of_term t) acc) ts empty 
		| Times (_,tl) -> 
		  List.fold_right (fun t acc ->  mult (!of_term t) acc) tl
		    (singleton (Base (make_map (), Map.make None)))
		| Not (_,x) -> begin
		  match x with
		    | Zero _ -> singleton (Base(make_map (),Map.make None))
		    | One _ -> empty
		    | Test (_,x,v) -> negate x v
		    | _ -> failwith "De Morgan law should have been applied"
		end
		| Star (_,x) ->
		  let s = !of_term x in
		  let s1 = add (Base(make_map (), Map.make None)) s in
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
