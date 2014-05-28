exception Quit
exception Undo

let debug_mode = false
let failed_Count = ref 0
let success_count = ref 1

module Field = struct 
  type t = int
  let compare = Pervasives.compare
  let hash x = x
  let equal a b = 0 = (compare a b)
  let of_string,to_string,max_elem = 
    let stringtoint = Hashtbl.create 11 in 
    let inttostring = Hashtbl.create 11 in 
    let counter = ref 0 in 
    let of_string (x : string) : t = 
      try Hashtbl.find stringtoint x 
      with Not_found -> 
	let id = !counter in 
	counter := !counter + 1 ;
	Hashtbl.replace stringtoint x id;
	Hashtbl.replace inttostring id x;
	id in 
    let to_string (x : t) : string = 
      Hashtbl.find inttostring x in 
    let max_elem _ = !counter in
    of_string,to_string,max_elem
end
module FieldSet = Set.Make(Field)
      
  
module Value = struct 
  type t = int
  let compare = Pervasives.compare
  let hash x = x
  let equal a b = 0 = (compare a b)
  let of_string,to_string,max_elem = 
    let stringtoint = Hashtbl.create 11 in 
    let inttostring = Hashtbl.create 11 in 
    let snowman =  "☃" in
    Hashtbl.replace stringtoint snowman (-1);
    Hashtbl.replace inttostring (-1) snowman;
    let counter = ref 0 in 
    let of_string (x : string) : t = 
      try Hashtbl.find stringtoint x 
      with Not_found -> 
	let id = !counter in 
	counter := !counter + 1 ;
	Hashtbl.replace stringtoint x id;
	Hashtbl.replace inttostring id x;
	id in 
    let to_string (x : t) : string = 
      Hashtbl.find inttostring x in 
    of_string,to_string,(fun _ -> !counter)
  let extra_val = -1
end
module ValueSet = Set.Make(Value) 

let all_fields = ref (fun _ -> failwith 
  "Please set all_fields in Decide_Util.ml before trying to run any calculations!")
let all_values = ref (fun _ -> failwith 
  "Please set all_values in Decide_Util.ml before trying to run any calculations!")

module FieldArray = struct
  type 'a t = 'a array
  let make (a : 'a) : 'a t = 
    let _ = !all_fields () in 
    Array.make (Field.hash (Field.max_elem ())) a
  let init f = 
    let _ = !all_fields () in 
    Array.init (Field.hash (Field.max_elem ())) f
  let set this k = 
    Array.set this (Field.hash k)
  let get this k = 
    Array.get this (Field.hash k)
  let fold f arr acc =
    let accr = ref acc in 
    Array.iteri (fun indx elem -> 
      let acc = !accr in 
      accr := (f indx elem acc)) arr;
    !accr
  let copy = Array.copy
end 
module ValueArray = struct
  type 'a t = 'a array
  let make (a : 'a) : 'a t = 
    let _ = !all_fields () in 
    Array.make (Value.hash (Value.max_elem ())) a
  let set this k = 
    Array.set this (Value.hash k)
  let get this k = 
    Array.get this (Value.hash k)
end

  
let output_endline (out : out_channel) (s : string) : unit =
  output_string out s;
  output_char out '\n'
  
let copy_lines in_channel out_channel : unit =
  try
    while true do
      output_endline out_channel (input_line in_channel)
    done
  with End_of_file -> ()
  
let rec range (min : int) (max : int) : int list =
  if max <= min then [] else min :: range (min + 1) max  
  
let rec remove_duplicates list =
  match list with
  | [] -> []
  | x :: t -> x :: remove_duplicates (List.filter ((<>) x) t)

(* perform f on all pairs *)
let cross (f : 'a -> 'b -> 'c) (s : 'a list) (t : 'b list) : 'c list =
  List.concat (List.map (fun x -> List.map (f x) t) s)

    
let thunkify f = 
  let ret = ref None in 
  (fun _ -> 
    match !ret with 
      | None -> let v = f() in ret := Some v; v
      | Some v -> v)


(*****************************************************
 * A functional version
 *****************************************************)
module type SetMapF =
  functor (K : Map.OrderedType) ->
  functor (V : Set.OrderedType) -> sig
    type t
    type elt = V.t
    module Values : Set.S with type elt = elt
    type eltSet = Values.t
    type key = K.t
    val empty : t
    val add : key -> elt -> t -> t
    val add_all : key -> eltSet -> t -> t
    val remove : key -> elt -> t -> t
    val remove_all : key -> t -> t
    val find_all : key -> t -> eltSet
    val contains_key : key -> t -> bool
    val contains_value : key -> elt -> t -> bool
    val size : key -> t -> int
    val keys : t -> key list
    val bindings : t -> (key * elt list) list
    (* val iter : (elt -> unit) -> key -> t -> unit     *)
    val iter : (key -> elt -> unit) -> t -> unit
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val fold : (key -> elt -> 'b -> 'b) -> t -> 'b -> 'b
    val fold_key : (elt -> 'b -> 'b) -> key -> t -> 'b -> 'b
    val filter : (key -> elt -> bool) -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val consis : key -> elt -> t -> bool
    val single_mapping : key -> t -> bool
    val for_all : (key -> elt -> bool) -> t -> bool
    val is_empty : t -> bool
    val val_inter : eltSet -> eltSet -> eltSet
    val val_equal : eltSet -> eltSet -> bool
    val val_is_empty : eltSet -> bool
    val val_empty : eltSet
    val val_mem : elt -> eltSet -> bool
    val val_size : eltSet -> int
    val val_singleton : elt -> eltSet
    val maps_to_empty : key -> t -> bool
    val to_string : t -> (key -> string -> string, unit, string) format ->
      (elt list -> string list) -> string
  end

module SetMapF : SetMapF =
  functor (K : Map.OrderedType) ->
  functor (V : Set.OrderedType) -> struct
    module Values = Set.Make(V)
    module Keys = Map.Make(K) 
    type t = Values.t Keys.t
    type elt = Values.elt
    type eltSet = Values.t
    type key = Keys.key
    let empty = Keys.empty
    let contains_key = Keys.mem
    let contains_value x v h =
      contains_key x h && Values.mem v (Keys.find x h)
    let add x v h =
      let s = if contains_key x h then Keys.find x h else Values.empty in
      let t = Values.add v s in
      Keys.add x t h
    let add_all k es mp = 
      if Values.is_empty es 
      then mp
      else Keys.add k es mp
    let remove_all = Keys.remove
    let find_all k t = try Keys.find k t with Not_found -> Values.empty 
    let remove x v h =
      if contains_key x h then
        let s = Keys.find x h in
        let t = Values.remove v s in
        if Values.is_empty t then Keys.remove x h else
        Keys.add x t h
      else h
    let size x h =
      if contains_key x h then Values.cardinal (Keys.find x h) else 0
    let keys h = List.map fst (Keys.bindings h)
    let bindings h =
      let s = Keys.bindings h in
      List.map (fun (x,a) -> (x, Values.elements a)) s
    (* let iter f x h =                                         *)
    (*   if contains_key x h then Values.iter f (Keys.find x h) *)
    let iter f = Keys.iter (fun x -> Values.iter (f x))
    let equal = Keys.equal Values.equal
    let compare = Keys.compare Values.compare
    let fold f = Keys.fold (fun x -> Values.fold (f x))
    let fold_key f x h b =
      if contains_key x h then Values.fold f (Keys.find x h) b
      else b
    let filter f h =
      let g x v h = if f x v then add x v h else h in
      fold g h empty
    let union = fold add
    let inter m1 m2 = 
      fold (fun key elt acc -> 
	if contains_value key elt m2
	then add key elt acc
	else acc
      ) m1 empty

    let consis x v h =
      not (contains_key x h) || contains_value x v h               
    let for_all f =
      Keys.for_all (fun k a -> Values.for_all (f k) a)	
	
    let single_mapping k t = 
      try 
	(Values.cardinal (Keys.find k t)) = 1
      with Not_found -> true
	
    exception Matthew_wants_call_cc

    let is_empty m =
      try 
	fold (fun _ _ _ -> raise Matthew_wants_call_cc) m true
      with Matthew_wants_call_cc -> false

    let val_inter = Values.inter
    let val_equal = Values.equal
    let val_is_empty = Values.is_empty
    let val_empty = Values.empty
    let val_mem = Values.mem
    let val_size = Values.cardinal
    let val_singleton = Values.singleton
    let maps_to_empty k t = Values.is_empty (find_all k t)
    let to_string (ssm : t) op elt_to_string = 
      let s = bindings ssm in
      let f (x,a) = Printf.sprintf op x (String.concat "," (elt_to_string a)) in
      String.concat ";" (List.map f s)

  end

module Int = struct
  type t = int
  let compare = compare
end

module StringSetMap = SetMapF (String) (String)
  
(*****************************************************
 * Stream of strings in length-lexicographic order --
 * use to create new variable names
 *****************************************************)
module type LexStream = sig
  type t
  val make : unit -> t
  val next : t -> string
end

module LexStream : LexStream = struct
  type t = int list ref 
  
  let rec inc (s : int list) : int list =
    match s with
      | [] -> [Char.code 'a']
      | x :: t ->
          if x < Char.code 'z' then (x + 1) :: t
          else Char.code 'a' :: inc t
        
  let make() : t = ref [Char.code 'a']

  let next (h : t) : string =
    let l = !h in
    h := inc l;
    String.concat "" (List.map (String.make 1) (List.map Char.chr (List.rev l)))
end

module WorkList = functor (K : Set.OrderedType) -> 
struct 
  module S = Set.Make(K)
  type t = S.t * (K.t list)

  let add (e : K.t) (wl : t)  : t = 
    let set,worklist = wl in
    if S.mem e set
    then (
      wl)
    else S.add e set,e::worklist

  let singleton (e : K.t ) : t = 
    S.singleton e, [e]

  let is_empty wl : bool = 
    let set,wl = wl in
    match wl with 
      | [] -> true
      | _ -> false

  let hd (set,wl) : K.t = 
    List.hd wl

  let tl (set,wl) : t = set, List.tl wl

  let all_seen_items (set,_) = 
    S.elements set
    
end

module UnionFind = functor(Ord : Map.OrderedType) -> struct
  module FindMap = Map.Make(Ord)
  type union_find_ds = 
    | Intermediate_node of int * (union_find_ds ref) * int (* depth*)
    | Root_node of int * int ref (*label + maxdepth*)
    | Leaf_node of Ord.t * (union_find_ds ref);;
  
  let init_union_find _ : 
      ((union_find_ds ref -> union_find_ds ref -> bool)* 
	  (Ord.t -> union_find_ds ref) * 
	  (union_find_ds ref -> union_find_ds ref -> 
	   union_find_ds ref)) = 
    let hash = ref FindMap.empty in 
    let fresh_int = ref 0 in
    let rec get_parent = function 
      | Intermediate_node (l,p,d) -> 
	(
	  match !p with 
	    | Root_node _ -> p
	    | _ -> get_parent !p)
      | Leaf_node (_,p) -> 
	(match !p with 
	  | Root_node _ -> p
	  | Leaf_node _ -> failwith "leaf can't point to leaf!!! bad!!!"
	  | _ -> get_parent !p)
      | _ -> failwith "you have already gotten the parent."
    in
    let eq a b = 
      match (!a,!b) with 
	| (Root_node (l1,_),Root_node (l2,_)) -> l1 = l2
	| _ -> failwith "equality only defined on roots" in
    let find e = 
      try get_parent(FindMap.find e !hash)
      with Not_found -> 
	hash := (FindMap.add e 
		   (let cntr = !fresh_int in 
		    fresh_int := !fresh_int + 1;
		    Leaf_node(e, ref (Root_node (cntr, ref 1)))
		   ) !hash); 
	get_parent(FindMap.find e !hash) in
    let union c1 c2 = 
      match (!c1,!c2) with 
	| (Root_node (l1,d1), Root_node (l2,d2)) -> 
	  if l1 = l2 then c1
	  else if !d2 < !d1 then (*c1 is new root*)
	    (c2:= Intermediate_node (l2,c1,!d2); c1)
	  else if !d1 > !d2 then 
	    (c1:= Intermediate_node (l1,c2,!d1); c2)
	  else 
	    (d1:= !d1 + 1; c2:= Intermediate_node(l2,c1,!d2); c1)
	| _ -> failwith "call union on the root nodes please." in
    eq,find,union
      
end
      



