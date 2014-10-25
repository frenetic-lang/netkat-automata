G¯¯
let collection_to_string fold elt_to_string sep c =
  fold (fun x acc ->
    if acc = "" 
    then acc ^ elt_to_string x 
    else acc ^ sep ^ elt_to_string x) c ""
    		  
let values_to_string (vs:Decide_Util.ValueSet.t) : string = 
  Printf.sprintf "{%s}"
    (collection_to_string 
       Decide_Util.ValueSet.fold Decide_Util.Value.to_string ", " vs)
    
let fields_to_string (vs:Decide_Util.FieldSet.t) : string = 
  Printf.sprintf "{%s}"
    (collection_to_string 
       Decide_Util.FieldSet.fold Decide_Util.Field.to_string ", " vs)

module PosNeg = struct
  type t = 
      Pos of Decide_Util.Field.t * Decide_Util.ValueSet.t
    | Neg of Decide_Util.Field.t * Decide_Util.ValueSet.t
  let to_string = function
    | Pos (_,s) -> 
      values_to_string s
    | Neg (f,s) -> 
      values_to_string (Decide_Util.ValueSet.diff ((!Decide_Util.all_values ()) f) s)
	
  let empty (f:Decide_Util.Field.t) : t = Pos(f, Decide_Util.ValueSet.empty)
      
  let singleton (f: Decide_Util.Field.t) (e : Decide_Util.ValueSet.elt) : t = 
    Pos(f,Decide_Util.ValueSet.singleton e)
      
  let any (f:Decide_Util.Field.t) : t = Neg(f, Decide_Util.ValueSet.empty) 
      
  let add e pn = 
    match e,pn with 
      | e,Pos(f,s) -> Pos(f,Decide_Util.ValueSet.add e s)
      | e,Neg(f,s) -> Neg(f,Decide_Util.ValueSet.remove e s)
	
  let union pn1 pn2 = 
    match pn1,pn2 with 
      | Pos(f,s1),Pos(_,s2) -> Pos(f,Decide_Util.ValueSet.union s1 s2)
      | (Pos(f,s1),Neg(_,s2) | Neg(_,s2),Pos(f,s1)) -> 
	Neg(f,Decide_Util.ValueSet.diff s2 s1)
      | Neg(f,s1), Neg(_,s2) -> Neg(f,Decide_Util.ValueSet.inter s1 s2)
	
  (* pre: pn1 and pn2 should be defined over the same field *)
  let compare pn1 pn2 = 
    match pn1, pn2 with 
      | Pos (_,s1), Pos (_,s2) -> 
        Decide_Util.ValueSet.compare s1 s2
      | Neg (_,s1), Neg (_,s2) -> 
        -1 * Decide_Util.ValueSet.compare s1 s2
      | Pos (_,s1), Neg (f,s2) -> 
        Decide_Util.ValueSet.compare s1 (Decide_Util.ValueSet.diff ((!Decide_Util.all_values ()) f) s2)
      | Neg (f,s1), Pos (_,s2) -> 
        Decide_Util.ValueSet.compare (Decide_Util.ValueSet.diff ((!Decide_Util.all_values ()) f) s1) s2
          
  let contains (pn:t) (v:Decide_Util.Value.t) : bool = 
    match pn with 
      | Pos(_,s) -> 
        Decide_Util.ValueSet.mem v s 
      | Neg(_,s) -> 
        not (Decide_Util.ValueSet.mem v s)
          
  let intersect (pn1:t) (pn2:t) : t = 
    match pn1, pn2 with
      | Pos(f,s1), Pos(f',s2) when f = f' -> 
        Pos(f,Decide_Util.ValueSet.inter s1 s2)
      | Neg(f,s1), Neg(f',s2) when f = f' -> 
        Neg(f,Decide_Util.ValueSet.union s1 s2)
      | Pos(f,s1), Neg(f',s2) when f = f' -> 
        Pos(f,Decide_Util.ValueSet.diff s1 s2)
      | Neg(f,s1), Pos(f',s2) when f = f' -> 
        Pos(f,Decide_Util.ValueSet.diff s2 s1)
      | _ -> failwith "intersecting two posnegs with non-equal field!"
	
  let is_empty (pn:t) : bool = 
    match pn with 
      | Pos(_,s) -> 
        Decide_Util.ValueSet.is_empty s
      | Neg(f,s) -> 
        Decide_Util.ValueSet.equal ((!Decide_Util.all_values ()) f) s
	  
  let subset small big : bool = 
    let open Decide_Util.ValueSet in 
	match small,big with 
          | Pos(f,s1), Pos(f',s2) when f = f' -> 
	    subset s1 s2
          | Neg(f,s1), Neg(f',s2) when f = f' -> 
	    subset s2 s1
          | Pos(f,s1), Neg(f',s2) when f = f' -> 
	    (cardinal (inter s1 s2)) = 0
          | Neg(f,s1), Pos(f',s2) when f = f' -> 
	    equal (union s1 s2) ((!Decide_Util.all_values ()) f)
	  | _ -> failwith "subset-testing two posnegs with non-equal field!"
	    
	    
  let elements (pn : t) : Decide_Util.ValueSet.t = 
    match pn with 
      | Pos (_,elts) -> elts
      | Neg (f,elts) -> 
	(Decide_Util.ValueSet.diff ((!Decide_Util.all_values ()) f) elts)
	  
end (* PosNeg *)
module Base = struct
    
  module Map = struct 
    type key = Decide_Util.Field.t
    type 'a t = (('a option) Decide_Util.FieldArray.t) 
    let find (a : key) (b : 'a t) : 'a = 
      try (match Decide_Util.FieldArray.get b a with 
	| Some r -> r
	| None -> raise Not_found)
      with Invalid_argument "index out of bounds" -> 
	Printf.eprintf "Requested key: %s\nAs int:%u\nSize: %u\n" 
	  (Decide_Util.Field.to_string a)
	  (Decide_Util.Field.as_int a)
	  (Decide_Util.FieldArray.size b);
	failwith "no idea"
	  
    let option_find a b = 
      Decide_Util.FieldArray.get b a
	
    let get a b = Decide_Util.FieldArray.get b a
      
    let fold (f : key -> 'a -> 'b -> 'b) (st : 'a t) (acc : 'b) : 'b = 
      Decide_Util.FieldArray.fold 
	(fun indx b acc -> 
	  match b with 
	    | Some e -> (f indx e acc)
	    | None -> acc) st acc 
	
    let fold_all_keys (f : key -> 'a option -> 'b -> 'b) (st : 'a t) (acc : 'b) : 'b = 
      Decide_Util.FieldArray.fold f st acc
	
    let fold2_all_keys 
	(f : key -> 'a option -> 'c option -> 'b -> 'b) (st1 : 'a t) (st2 : 'c t) (acc : 'b) : 'b = 
      Decide_Util.FieldArray.fold 
	(fun fld e1 -> let e2 = option_find fld st2 in f fld e1 e2)
	st1 acc
	

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
      Decide_Util.FieldSet.fold 
	(fun fld acc -> 
	  if acc = 0
	  then 
	    match Decide_Util.FieldArray.get a fld,(Decide_Util.FieldArray.get b fld) with 
	      | Some a', Some b' -> 
		cmpr a' b'
	      | a',b' -> 
		Pervasives.compare a' b'
	  else acc) (!Decide_Util.all_fields ()) 0 
	
    let add (a : key) (b : 'a) (arr : 'a t) : 'a t = 
      let newarr = Decide_Util.FieldArray.copy arr in 
      Decide_Util.FieldArray.set newarr a (Some b);
      newarr 
	
    let add' (a : key) (b : 'a option) (arr : 'a t) : 'a t = 
      let newarr = Decide_Util.FieldArray.copy arr in 
      Decide_Util.FieldArray.set newarr a b;
      newarr 


    let init f = Decide_Util.FieldArray.init 
      (fun a -> 
	if (Decide_Util.FieldSet.mem a (!Decide_Util.all_fields ()))
	then Some (f a)
	else None
      )
	

    let init' f = Decide_Util.FieldArray.init
      (fun a -> 
	if (Decide_Util.FieldSet.mem a (!Decide_Util.all_fields ()))
	then (f a)
	else None
      )

    let singleton k v = 
      init' (fun k' -> if k = k' then Some v else None)

    let empty : unit -> 'a t = (fun _ -> (Decide_Util.FieldArray.make None))

  end

  type atom = PosNeg.t Map.t
  type assg = Decide_Util.ValueSet.elt Map.t
  let (atom_empty : unit -> atom) = Map.empty
  let (assg_empty : unit -> assg) = Map.empty
    
  let atom_to_string (a:atom) : string = 
    Decide_Util.FieldSet.fold (fun f acc -> 
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
      (!Decide_Util.all_fields ()) ""
        
  let assg_to_string (b:assg) : string = 
    Decide_Util.FieldSet.fold (fun f acc -> 
      let vstr = 
	try Decide_Util.Value.to_string (Map.find f b)
	with Not_found -> "_"
      in
      Printf.sprintf "%s%s:=%s"
        (if acc = "" then acc else acc ^ ", ") 
        (Decide_Util.Field.to_string f)
        vstr)
      (!Decide_Util.all_fields ()) ""
        
  let atom_compare (a1:atom) (a2:atom) : int = 
    Map.compare PosNeg.compare a1 a2

  let assg_compare (b1:assg) (b2:assg) : int = 
    Map.compare Decide_Util.Value.compare b1 b2

	
  let atom_subset (a : atom) (b : atom) : bool = 
    Map.fold_all_keys
      (fun f a acc -> 
	let b = Map.option_find f b in 
	match a,b with 
	  | Some a, Some b -> PosNeg.subset a b && acc
	  | _, None -> acc
	  | None, Some e -> (PosNeg.compare e (PosNeg.any f)) = 0 && acc
      ) a true


  let atom_union (a : atom) (b : atom) : atom = 
    Map.fold2_all_keys
      (fun f a' b' acc -> 
	Map.add' 
	  f (match a',b' with 
	    | Some a'', Some b'' -> Some (PosNeg.union a'' b'')
	    | (None,_ | _, None) -> None ) acc) a b (Map.empty ())


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
    Decide_Util.FieldSet.fold (fun f acc -> 
      let vstr = 
	try Decide_Util.Value.to_string (Map.find f b)
	with Not_found -> "_"
      in
      Printf.sprintf "%s%s=%s"
        (if acc = "" then acc else acc ^ ", ") 
        (Decide_Util.Field.to_string f)
        vstr)
      (!Decide_Util.all_fields ()) ""

  let complete_test_vals (b : complete_test) : 
      (Decide_Util.Field.t * Decide_Util.Value.t) list = 
    Decide_Util.FieldSet.fold (fun f acc -> 
      let v = 
	try Map.find f b
	with Not_found -> 
	  failwith "this was supposed to be a complete test..." in
      (f,v) :: acc
    ) (!Decide_Util.all_fields ()) []

  let point_to_string (Point(x,y)) = 
    Printf.sprintf "<%s,%s>" (complete_test_to_string x) (assg_to_string y)

  let point_rhs (Point(_,r)) = r
  let point_lhs (Point(l,_)) = l

  let complete_test_to_point l r = Point(l,r)

  let to_string (Base(a,b) : t) : string =
    Printf.sprintf "<%s;%s>" (atom_to_string a) (assg_to_string b)

  let bto_string = to_string 
        
  let compare (Base(a1,b1):t) (Base(a2,b2):t) : int =
    let cmp = assg_compare b1 b2 in
    if cmp <> 0 then cmp else atom_compare a1 a2

  let compar = compare
    
  let equal (x:t) (y:t) : bool = 
    compare x y = 0

  type this_t = t
  module S = Decide_Set.Make(struct
    type t = this_t
    let compare = compare
    let equal = equal
  end)

  let univ_base _ = Base(atom_empty (), assg_empty ())
      
  exception Empty_mult

  let base_of_point (Point(x,y) : point) : t = 
    let x = 
      Map.init
	(fun f -> 
	  match Map.get f x with 
	    | Some v -> (PosNeg.Pos(f,Decide_Util.ValueSet.singleton v))
	    | None -> 
	      (Printf.eprintf "Point doen't have this field: %s\n" (Decide_Util.Field.to_string f); 
	       Printf.eprintf "This was the point: %s\n" (point_to_string (Point(x,y)));
	       Printf.eprintf "The universe is: %s\n" (to_string (univ_base ()));
	       Printf.eprintf "all_fields is: %s\n" 
		 (Decide_Util.FieldSet.fold 
		    (fun y -> Printf.sprintf "%s %s" (Decide_Util.Field.to_string y)) 
		    (!Decide_Util.all_fields ()) "");
	       failwith "point doesn't match the spec: base_of_point")) in 
    Base(x,y)
      
      
  let contains_point (Point(x,y) : point) (Base(a,b) : t) : bool = 
    Decide_Util.FieldSet.fold 
      (fun field acc -> 
	let x = try Map.find field x with Not_found -> 
	  failwith "Point doesn't match the spec: contains_point 1." in
	let y = try Map.find field y with Not_found -> 
	  failwith "Point doesn't match the spec: contains_point 2." in
	let a = try Map.find field a with Not_found -> 
	  (PosNeg.any field) in
	PosNeg.contains a x && 
	  (try y = (Map.find field b)
	   with Not_found -> y = x)
	&& acc
      ) (!Decide_Util.all_fields ()) true


  exception Empty_filter


  let fold_points (f : (point -> 'a -> 'a)) (Base(a,b) : t) (acc : 'a) 
      : 'a =
    let extract_points bse : point list = 
      Decide_Util.FieldSet.fold
	(fun field partial_list -> 
	  let a = PosNeg.elements (try Map.find field a 
	    with Not_found -> 
	      (PosNeg.any field)) in
	  List.fold_left
	    (fun (acc : point list) (Point(x,y)) -> 
	      try 
		let b = Map.find field b in
		Decide_Util.ValueSet.fold (fun v acc -> 
		  (Point(Map.add field v x, Map.add field b y)) :: acc
		) a acc
	      with Not_found ->		    
		Decide_Util.ValueSet.fold (fun v acc -> 
		  (Point(Map.add field v x, Map.add field v y)) :: acc
		) a acc
	    ) [] partial_list
	) (!Decide_Util.all_fields ()) [Point((assg_empty ()), (assg_empty ()))]
    in
    let g a p = f p a in 
    let pts = (extract_points b) in
    List.fold_left g acc pts
      
      
  let union ( a :t ) (b : t) : t list = 
    let get_pn_field = function 
    (PosNeg.Pos(f,_) | PosNeg.Neg(f,_)) -> f in 
    let all_but_one_match a' b' = 
      snd 
	(Map.fold2_all_keys
	   (fun f ae be (seen_diff,acc) -> 
	     if seen_diff
	     then match ae,be with 
	       | Some ae', Some be' -> seen_diff,acc && ((PosNeg.compare ae' be') = 0)
	       | (None, Some e' | Some e',None) -> 
		 seen_diff,acc && ((PosNeg.compare e' (PosNeg.any (get_pn_field e'))) = 0)
	       | None, None -> seen_diff,acc
	     else match ae,be with 
	       | Some ae', Some be' -> ((PosNeg.compare ae' be') <> 0),acc
	       | (None, Some e' | Some e',None) -> 
		 ((PosNeg.compare e' (PosNeg.any (get_pn_field e'))) <> 0),acc
	       | None, None -> seen_diff,acc
	   ) a' b' (false,true)) in 
    let Base(al,ar) = a in 
    let Base(bl,br) = b in 
    if assg_compare ar br <> 0 then [a;b]
    else if (atom_subset al bl) || atom_subset bl al || all_but_one_match al bl 
    then let res = Base(atom_union al bl,ar) in 
	 if Decide_Util.debug_mode
	 then (assert 
		 (fold_points (fun pt acc -> contains_point pt res && acc) a true
		  && fold_points (fun pt acc -> contains_point pt res && acc) b true
		  && fold_points (fun pt acc -> (contains_point pt a || contains_point pt b)  && acc) res true));
	 [res]
    else [a;b]
	    
  let bunion = union 

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
      Some 
	(Decide_Util.FieldSet.fold 
           (fun field (Base(a,b)) ->
	     let pn1 = try Map.find field a1 with Not_found -> 
	       PosNeg.any field in 
	     let pn2 = try Map.find field a2 with Not_found -> 
	       PosNeg.any field in           
	     let o1 = try Some (Map.find field b1) with Not_found -> None in 
	     let o2 = try Some (Map.find field b2) with Not_found -> None in 
	     let pn',o' = 
	       match o1,o2 with 
                 | (Some v1, Some v2) when PosNeg.contains pn2 v1 -> (pn1, o2)
                 | (Some v1, None) when PosNeg.contains pn2 v1 -> (pn1, o1)
		 | (None, _) -> 
		   let inter = (PosNeg.intersect pn1 pn2) in
		   if (PosNeg.is_empty inter) then raise Empty_mult;
		   inter,o2 
		     
		 | _ -> raise Empty_mult in
	     Base(Map.add field pn' a, 
                  match o' with
                    | None -> b
                    | Some v' -> Map.add field v' b))
           ((!Decide_Util.all_fields ())) (Base((atom_empty ()), (assg_empty ()))))
    with Empty_mult -> 
      None
	
  let project_lhs (Base(a,b)) = 
    Base(a,(assg_empty ()))

  let of_assg field v = Base((atom_empty ()), Map.singleton field v )

  let of_test field v = 
    Base(Map.singleton field (PosNeg.Pos (field,Decide_Util.ValueSet.singleton v)), assg_empty ())

  let of_neg_test x v = 
    Base(
      Map.singleton x (PosNeg.Neg(x,Decide_Util.ValueSet.singleton v)), 
      assg_empty ())
	
  module Set = struct
    include S

    let to_string (bs:t) : string = 
      Printf.sprintf "{%s}" 
        (S.fold 
	   (fun x s -> 
	     (if s = "" then s else s ^ ",\n") ^ to_string x) bs "")
	  

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

    let fold_points (f : (point -> 'a -> 'a)) (st : t) (acc : 'a) : 'a =
      snd (fold 
	     (fun base pacc -> 
	       fold_points (fun elt (seen_points,acc) -> 
		 let belt = base_of_point elt in 
		 if S.mem belt seen_points
		 then seen_points,acc
		 else (S.add belt seen_points, f elt acc))
		 base pacc)
	     st (S.empty,acc))

	
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


    (* is it faster to check all things with equal RHS first and then restart, 
       or should you restart as soon as you've succeeded on a merge? *)

    let compare_rhs (Base(_,a)) (Base(_,b)) = assg_compare a b
      
    let merge_element : this_t -> t -> t = (fun a e' -> 
      let a = ref a in 
      let to_remove = 
	(fold_range 	    
	   (* minimal base with equal RHS *)
	   (fun e -> match compare_rhs e !a with 
	     | -1 -> false
	     | (0 | 1) -> true
	     | _ -> failwith "bad compare function")
	   (* maximal base with equal RHS *)
	   (fun e -> match compare_rhs e !a with 
	     | (-1 | 0) -> true 
	     | 1 -> false
	     | _ -> failwith "bad compare function" )
	   (fun b acc -> 
	     assert (compare_rhs !a b = 0);
	     match bunion !a b with 
	       | [a'] -> let olda = !a in a:= a'; (add olda (add b acc))
	       | [a;b] -> acc
	       | _ -> failwith "bunion didn't work like i wanted.") 
	   e' empty ) in 
      add !a (diff e' to_remove)
    )

    let rec compact e = 
      let old_cardinal = cardinal e in 
      let e' = fold merge_element e e  in 
      if Decide_Util.debug_mode 
      then (assert (equal e e');
	    assert (old_cardinal >= (cardinal e'))
      );
      if old_cardinal = (cardinal e')
      then e' 
      else compact e'
	  
    let union (a : t) (b : t) : t = 
      let res = union a b in 
      res

    let add a b = 
      let res = 
	if mem a b then b
	else merge_element a b in 
      res
(*


      let add b s : t = 
	let res = match fold 
	    (fun e (acc,s') -> 
	      match bunion e b with 
		| [a;b] -> acc,add e s' 
		| [a] -> None, add a s'
		| _ -> failwith "shouldn't happen") s (Some b, empty)
	  with 
	    | Some b, s' -> add b s' 
	    | None, s' -> s' in 

	if Decide_Util.debug_mode then 
	  (let olds = add b s in 
	  assert (equal olds res));
	res (* compact res *)
*)

    let mult (left : t)  (right : t) : t = 
      let open Decide_Util in 
      let module ValHash = Hashtbl.Make(Decide_Util.Value) in
      let extract_test (f : Decide_Util.Field.t) (Base(atom,_)) = 
	try Map.find f atom 
	with Not_found -> PosNeg.any f in 
      let pn_intersect (a : Decide_Util.ValueSet.t) (b : PosNeg.t) : Decide_Util.ValueSet.t = 
	match a, b with
          | s1, PosNeg.Pos(_,s2) -> 
            Decide_Util.ValueSet.inter s1 s2
          | s1, PosNeg.Neg(_,s2) -> 
            Decide_Util.ValueSet.diff s1 s2 in
      let intersect_all lst = 
	List.fold_left
	  (fun acc e -> inter e acc) (List.hd lst) (List.tl lst) in 
      let incr_add k v hash = 
	ValHash.replace hash k (add v (ValHash.find hash k));
	hash in
      let incr_add_all ks v hash = 
	Decide_Util.ValueSet.iter 
	  (fun k -> 
	    ValHash.replace hash k (add v (ValHash.find hash k))) ks;
	hash in

      let phase1 = fold 
	(fun b  acc -> 
	  let Base(test,assg) = b in
	  List.map
	    (fun (field,valset,valhash,others) -> 
	      try 
		let a = Map.find field assg in 
		field,Decide_Util.ValueSet.add a valset,incr_add a b valhash, others
	      with Not_found -> 
		try 
		  let a = Map.find field test in 
		  match a with 
		    | PosNeg.Pos(_,a) -> field,
		      Decide_Util.ValueSet.union a valset,incr_add_all a b valhash, others
		    | _ -> raise Not_found
		with Not_found -> 
		  field,valset,valhash,add b others) acc)
	left (Decide_Util.FieldSet.fold 
		 (fun f acc -> 
		   (f,Decide_Util.ValueSet.empty,ValHash.create 1000,empty)::acc) 
		 (!Decide_Util.all_fields ()) []) in

      let phase2 = 
	fold 
	  (fun b acc -> 
	    let to_intersect = 
	      List.map
		(fun (f,vs,vh,bs) -> 
		  let i = pn_intersect vs (extract_test f b) in 
		  union 
		    (Decide_Util.ValueSet.fold 
		       (fun a -> union (ValHash.find vh a)) i empty) 
		    bs) phase1 in 
	    (b,intersect_all to_intersect)::acc) right [] in 

	(* TODO: take (lhs - phase2) and do mult with that as well.  assert that 
	   the result is empty for each pair.  If it's not empty, print out the pair that 
	   is non-empty.
	*)

      let phase3 = 
	List.fold_left
	  (fun acc (rhs,cnds) -> 
	    fold (fun lhs acc -> 
	      match (mult lhs rhs) with 
		| Some r -> add r acc
		| None -> acc
	    ) cnds acc) empty phase2 in 
      
      if Decide_Util.debug_mode 
      then (
	let old_res = (old_mult left right) in
	if not (equal old_res phase3)
	then (Printf.eprintf "LHS of mult: %s\nRHS of mult: %s\nNew result: %s\nOld result: %s\n"
		(to_string left) 
		(to_string right) 
		(to_string (compact (compact (compact (compact phase3)))))
		(to_string (compact (compact (compact (compact old_res)))));
	      failwith "new mult and old mult don't agree!");
	phase3)
      else phase3



    let biggest_cardinal = ref 0 

      
    let filter_alpha bs (beta : complete_test) = 
      fold
	(fun b acc -> 
	  match (filter_alpha b beta) with 
	    | None -> acc
	    | Some r -> add r acc)
	bs empty


    let print_debugging_info _ = ()

  end (* Base.Set *)	    

end (* Base *)
