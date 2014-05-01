module type UnivDescr = sig 
  type value 
  type field
  module FieldSet : Set.S with type elt = field
  module ValueSet : Set.S with type elt = value
  val field_compare : field -> field -> int
  val value_compare : value -> value -> int
  val all_fields : FieldSet.t
  val all_values : field -> ValueSet.t
  val field_to_string : field -> string
  val value_to_string : value -> string
  val field_of_id : Ast.id -> field
  val value_of_id : Ast.id -> value
  val value_of_string : string -> value

end

let collection_to_string fold elt_to_string sep c =
  fold (fun x acc ->if acc = "" then acc ^ elt_to_string x else acc ^ sep ^ elt_to_string x) c ""

module Univ = functor (U : UnivDescr) -> struct 
  
  let values_to_string (vs:U.ValueSet.t) : string = 
    Printf.sprintf "{%s}"
      (collection_to_string U.ValueSet.fold U.value_to_string ", " vs)

  let fields_to_string (vs:U.FieldSet.t) : string = 
    Printf.sprintf "{%s}"
      (collection_to_string U.FieldSet.fold U.field_to_string ", " vs)

  module PosNeg = struct
    type t = 
        Pos of U.field * U.ValueSet.t
      | Neg of U.field * U.ValueSet.t
    let to_string = function
      | Pos (_,s) -> 
        values_to_string s
      | Neg (f,s) -> 
        values_to_string (U.ValueSet.diff (U.all_values f) s)

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

    module Map = Map.Make(struct
      type t = U.field 
      let compare = U.field_compare 
    end)      
    type atom = PosNeg.t Map.t
    type assg = U.ValueSet.elt Map.t
                
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
          (U.field_to_string f)
          pnstr)
        U.all_fields ""
        
    let assg_to_string (b:assg) : string = 
      U.FieldSet.fold (fun f acc -> 
	let vstr = 
	  try U.value_to_string (Map.find f b)
	  with Not_found -> "_"
	in
        Printf.sprintf "%s%s:=%s"
          (if acc = "" then acc else acc ^ ", ") 
          (U.field_to_string f)
          vstr)
        U.all_fields ""
        
    let atom_compare (a1:atom) (a2:atom) : int = 
      Map.compare PosNeg.compare a1 a2

    let assg_compare (b1:assg) (b2:assg) : int = 
      Map.compare U.value_compare b1 b2

    type t = Base of atom * assg 
    (* must be a Pos * completely-filled-in thing*)
    type point = Point of assg * assg

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
	  let a = try Map.find field a with Not_found -> (PosNeg.any field) in
	  PosNeg.contains a x && 
	    (try y = (Map.find field b)
	     with Not_found -> y = x)
	  && acc
	) U.all_fields true


    let test_of_point_left (Point(x,_) : point) : Ast.term = 
      Ast.Term.Times
	(U.FieldSet.fold 
	   (fun field acc -> 
	     let v = try Map.find field x with Not_found -> 
	       failwith "Point doesn't match the spec."  in
	     (Ast.Term.Test(U.field_to_string field, U.value_to_string v))::acc)
	   U.all_fields [])

    let test_of_point_right (Point(_,y) : point) : Ast.term = 
      Ast.Term.Times
	(U.FieldSet.fold 
	   (fun field acc -> 
	     let v = try Map.find field y with Not_found -> 
	       failwith "Point doesn't match the spec."  in
	     (Ast.Term.Test(U.field_to_string field, U.value_to_string v))::acc)
	   U.all_fields [])

    exception Empty_filter

    (* TODO: is this right? mpm *)
    let filter_alpha (Base(a1,b1):t) (a2: atom) : t option =
      try
	Some (U.FieldSet.fold
		(fun field (Base(tests, assgs)) ->
		  let test_1 =
		    try Map.find field a1 with Not_found -> PosNeg.any field in
		  let test_2 =
		    try Map.find field a2 with Not_found -> PosNeg.any field in
		  let new_test = PosNeg.intersect test_1 test_2 in
		  if PosNeg.is_empty new_test
		  then raise Empty_filter;
		  let new_assg = (try Some (Map.find field b1) with Not_found -> None) in
		  match new_assg with
		    | None -> Base(Map.add field new_test tests, assgs)
		    | Some new_assg ->
		      Base(Map.add field new_test tests,
			   Map.add field new_assg assgs))
		U.all_fields (Base(Map.empty, Map.empty)))
      with Empty_filter -> None

    let mult (Base(a1,b1):t) (Base(a2,b2):t) : t option = 
      try 
        Some (U.FieldSet.fold 
          (fun field (Base(a,b)) ->
            let pn1 = try Map.find field a1 with Not_found -> PosNeg.any field in 
            let pn2 = try Map.find field a2 with Not_found -> PosNeg.any field in           
            let o1 = try Some (Map.find field b1) with Not_found -> None in 
            let o2 = try Some (Map.find field b2) with Not_found -> None in 
            let pn',o' = 
              match o1,o2 with 
                | (Some v1, Some v2) when PosNeg.contains pn2 v1 -> 
                  (pn1, o2)
                | (Some v1, None) when PosNeg.contains pn2 v1 -> 
                  (pn1, o1)
                | (None, Some v2) when not (PosNeg.is_empty (PosNeg.intersect pn1 pn2)) -> 
                  (PosNeg.intersect pn1 pn2, o2)
                | None, None when not (PosNeg.is_empty (PosNeg.intersect pn1 pn2)) -> 
                  (PosNeg.intersect pn1 pn2, None) 
                | _ -> 
                  raise Empty_mult in 
            Base(Map.add field pn' a, 
                 match o' with
                   | None -> b
                   | Some v' -> Map.add field v' b))
          (U.all_fields) (Base(Map.empty, Map.empty)))
      with Empty_mult -> 
        None

    let fold_points (f : (point -> 'a -> 'a)) (Base(a,b) : t) (acc : 'a) : 'a =
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
	    ) U.all_fields [Point(Map.empty, Map.empty)]
      in
      let pts = (extract_points b) in
      List.fold_right f pts acc

    let project_lhs (Base(a,b)) = 
      Base(a,Map.empty)
	
    module Set = struct
      include S

      let to_string (bs:t) : string = 
        Printf.sprintf "{%s}" 
          (S.fold (fun x s -> (if s = "" then s else s ^ ", ") ^ to_string x) bs "")
	  

      (* TODO: a more efficient multiplication would be nice.*)
      let mult (left : t) (right : t) : t =
	let f x y (r : t) : t =
          match mult x y with
            | Some z -> add z r
            | None -> r in
	let g x  (r : t) : t = fold (f x) right r in
	fold g left empty

      let contains_point (st : t) (pt : point) : bool = 
	fold (fun e acc -> (contains_point pt e) || acc) st false
	
      (* we're dealing with duplicates right now by rememering and skipping points.  
	 It would be better to invent a normal form which guaranteed no duplicates.
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

      let compare a b = failwith "can't do it"

      let equal (a : t) (b : t) = 
	fold_points (fun pt acc -> contains_point b pt && acc) a true
	&& fold_points (fun pt acc -> contains_point a pt && acc) b true


    (* of_term : Ast.term -> Set.t *)
    (* this calculates the E matrix *)
    let rec of_term (t0:Ast.term) : t = 
      (* to negate a test x=v, we allow x to have any value except v *)
      let negate (x : U.field) (v : U.value) =
	singleton(Base(Map.add x (PosNeg.Neg(x,U.ValueSet.singleton v)) Map.empty,Map.empty))
      in
      let t0 = Ast.zero_dups t0 in (* may only normalize dup-free terms *)
      let t0 = Ast.deMorgan t0 in
      let open Ast.Term in 
      match t0 with 
        | One -> 
          singleton (Base(Map.empty, Map.empty))
        | Zero -> 
          empty
        | Assg(field,v) -> 
          singleton (Base(Map.empty, Map.add (U.field_of_id field) (U.value_of_id v) Map.empty))
	| Test(field,v) ->  
	  let field = U.field_of_id field in
	  let v = U.value_of_id v in
	  singleton (Base(Map.add field (PosNeg.Pos (field,U.ValueSet.singleton v)) Map.empty, Map.empty))
        | Dup -> 
          empty
        | Plus ts ->
          Ast.TermSet.fold (fun t acc -> union (of_term t) acc) ts empty 
        | Times tl -> 
          List.fold_right (fun t acc ->  mult (of_term t) acc) tl
	    (singleton (Base (Map.empty, Map.empty)))
	| Not x -> begin
	  match x with
	    | Zero -> singleton (Base(Map.empty,Map.empty))
	    | One -> empty
	    | Test (x,v) -> negate (U.field_of_id x) (U.value_of_string v)
	    | _ -> failwith "De Morgan law should have been applied"
	end
	| Star x ->
	  let s = of_term x in
	  let s1 = add (Base(Map.empty, Map.empty)) s in
	  let rec f (s : t) (r : t) : t =
            if equal s r then s
            else f (mult s s) s in
	  f (mult s1 s1) s1

    let of_term = Util.memoize of_term

    let filter_alpha bs complete_test = 
      match (elements (of_term complete_test)) with 
	| [Base(beta,_(*should be beta*))] -> 
	  fold
	    (fun b acc -> 
	      match (filter_alpha b beta) with 
		| None -> acc
		| Some r -> add r acc)
	    bs empty
	| _ -> Printf.printf "complete test we got as input: %s\n" (Ast.term_to_string complete_test); 
	  Printf.printf "BaseSet we got: %s \n" (to_string (of_term complete_test));
	  failwith "in filter_beta, that wasn't a single complete test!"


    end (* Base.Set *)	    

  end (* Base *)
end (* Univ *)
