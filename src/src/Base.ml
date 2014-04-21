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

end

let collection_to_string fold elt_to_string sep c =
  fold (fun x acc -> if acc = "" then acc else acc ^ sep ^ elt_to_string x) c ""

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
      | Neg (_,s) -> 
        "!" ^ values_to_string s 

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
  end (* PosNeg *)
  module Base = struct

    module Map = Map.Make(struct
      type t = U.field 
      let compare = U.field_compare 
    end)      
    type atom = PosNeg.t Map.t
    type assg = U.ValueSet.elt Map.t
                
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
      Map.compare PosNeg.compare a1 a2

    let assg_compare (b1:assg) (b2:assg) : int = 
      Map.compare U.value_compare b1 b2

    type t = Base of atom * assg 
    type point = t

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
      
    (* Operations on Set.t *)
    exception Empty_mult

    let is_empty (Base(a,b):t) : bool = 
      failwith "NYI"

    let intersect (Base(a1,b1):t) (Base(a2,b2):t) : t = 
      failwith "NYI"

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

      let fold_points (f : (point -> 'a -> 'a)) (st : t) (acc : 'a) : 'a =
	failwith "implme"

    (* of_term : Ast.term -> Set.t *)
    (* this calculates the E matrix *)
    let rec of_term (t0:Ast.term) : t = 
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
        | Not x -> 
          assert false
        | Star x -> 
          assert false

    let contains_point (st : t) (pt : point) : bool = 
      failwith "implme"

    end

	    
    let assg_of_point (p : point) : Ast.term = 
      failwith "implme"

  end (* Base *)
end (* Univ *)
