open Decide_Util

exception Empty

let utf8 = ref false 

(***********************************************
						* syntax
***********************************************)

let biggest_int = ref 0  
  
type cached_info = 
    { e_matrix : unit -> Decide_Base.Base.Set.t;
      one_dup_e_matrix : unit -> Decide_Base.Base.Set.t 
    }

module rec TermSet : sig
  include Set.S
  val of_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val to_string : t -> string 
  val ts : (t -> string) ref
end with type elt = Term.t = struct
  include Set.Make (Term)
  let of_list (tl : elt list) : t =
    List.fold_right add tl empty
  let ts = let r : (t->string) = (fun _ -> failwith "backpatch") in ref r
  let to_string st = !ts st
end and Term : sig      
  type uid = int
      
  type t =
    | Assg of uid * Decide_Util.Field.t * Decide_Util.Value.t * cached_info option 
    | Test of uid * Decide_Util.Field.t * Decide_Util.Value.t * cached_info option 
    | Dup of uid * cached_info option 
    | Plus of uid * TermSet.t * cached_info option 
    | Times of uid * t list * cached_info option 
    | Not of uid * t * cached_info option 
    | Star of uid * t * cached_info option 
    | Zero of uid * cached_info option
    | One of uid * cached_info option
	
  (* pretty printers + serializers *)
  val to_string : t -> string
    
  (* utilities for Map, Hashtbl, etc *)
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
    
    
  (* because module troubles *)
  val extract_uid : t -> uid
end = struct 
  type uid = int	
      
  type t =
    | Assg of uid * Decide_Util.Field.t * Decide_Util.Value.t * cached_info option 
    | Test of uid * Decide_Util.Field.t * Decide_Util.Value.t * cached_info option 
    | Dup of uid * cached_info option 
    | Plus of uid * TermSet.t * cached_info option 
    | Times of uid * t list * cached_info option 
    | Not of uid * t * cached_info option 
    | Star of uid * t * cached_info option 
    | Zero of uid * cached_info option
    | One of uid * cached_info option
	
  let extract_uid = 
    function 
      | Assg (id,_,_,_)
      | Test (id,_,_,_)
      | Dup (id,_)
      | Plus (id,_,_)
      | Times (id,_,_)
      | Not (id,_,_)
      | Star (id,_,_)
      | Zero (id,_)
      | One (id,_)
	-> id 
  
  let rec to_string (t : t) : string =
    (* higher precedence binds tighter *)
    let out_precedence (t : t) : int =
      match t with
	| Plus _ -> 0
	| Times _ -> 1
	| Not _ -> 2
	| Star _ -> 3
	| _ -> 4 (* assignments and primitive tests *) in
    (* parenthesize as dictated by surrounding precedence *)
    let protect (x : t) : string =
      let s = to_string x in
      if out_precedence t <= out_precedence x then s else "(" ^ s ^ ")" in
    let assoc_to_string (op : string) (ident : string) (s : string list) 
	: string =
      match s with
	| [] -> ident
	  | _ -> String.concat op s in
    Printf.sprintf "#%d %s" (extract_uid t)
    (match t with
      | Assg (_, var, value,_) -> Printf.sprintf "%s:=%s" 
	(Field.to_string var) (Value.to_string value)
      | Test (_, var, value,_) -> Printf.sprintf "%s=%s" 
	(Field.to_string var) (Value.to_string value)
      | Dup _ -> "dup"
      | Plus (_,x,_) -> assoc_to_string " + " "0" (List.map protect 
						     ( TermSet.elements x ))
      | Times (_,x,_) -> assoc_to_string ";" "1" (List.map protect x)
      | Not (_,x,_) -> (if !utf8 then "¬" else "~") ^ (protect x)
      | Star (_,x,_) -> (protect x) ^ "*"
      | Zero _ -> "drop"
      | One _ -> "pass")
	  
	  
  let remove_cache = function
    | Assg (a,b,c,_) -> Assg(a,b,c,None)
    | Test (a,b,c,_) -> Test(a,b,c,None)
    | Dup (a,_) -> Dup(a,None)
    | Plus (a,b,_) -> Plus (a,b,None)
    | Times (a,b,_) -> Times (a,b,None)
    | Not (a,b,_) -> Not (a,b,None)
    | Star (a,b,_) -> Star (a,b,None)
    | Zero (a,_) -> Zero(a,None)
    | One (a,_) -> One(a,None)
      
  let compare a b = 
    match extract_uid a, extract_uid b with 
      | (-1,_ | _,-1) -> 
	failwith "error: comparing before setting uid"
      | uida,uidb -> 
	if Pervasives.compare uida uidb = 0
	then (Printf.printf "Equal terms: %s \n %s\n" (Term.to_string a) (Term.to_string b); 0 )
	else Pervasives.compare uida uidb
  let equal a b = 
    (compare a b) = 0
    
  let rec invalidate_id = function 
    | Assg (_,l,r,o) -> Assg(-1,l,r,o)
    | Test (_,l,r,o) -> Test(-1,l,r,o)
    | Plus (_,es,o) -> Plus(-1,TermSet.map invalidate_id es,o)
    | Times (_,es,o) -> Times(-1,List.map invalidate_id es,o)
    | Not (_,e,o) -> Not(-1,invalidate_id e,o)
    | Star (_,e,o) -> Star(-1,invalidate_id e,o)
    | other -> other
      
      
  let hash a = match extract_uid a with 
    | -1 -> Hashtbl.hash (invalidate_id a)
    | a -> Hashtbl.hash a
      
end 
    let _ = TermSet.ts:= (fun (ts : TermSet.t) -> 
      let l = TermSet.elements ts in
      let m = List.map Term.to_string l in
      String.concat "\n" m)

module TermMap = Map.Make(struct 
  type t = Term.t
  let compare = Term.compare
end
)

module TermPairSet = struct 
  include Set.Make(struct 
    type t = Term.t * Term.t
    let compare (al,ar) (bl,br) = 
      match Term.compare al bl with 
	| 0 -> Term.compare ar br
	| o -> o 
  end)
  let map f ts = 
    fold (fun (l,r) acc -> add (f (l,r)) acc) ts empty
  let bind ts f = 
    fold (fun (l,r) t -> union (f (l,r)) t) ts empty
end


 
open Term
type term = Term.t

module UnivMap = Decide_Util.SetMapF (Field) (Value)
 
type formula = Eq of Term.t * Term.t
	       | Le of Term.t * Term.t

open Decide_Base

let zero = Zero (0,Some {e_matrix = (fun _ -> Base.Set.empty);
			 one_dup_e_matrix = (fun _ -> Base.Set.empty)})
let one = One (1,Some {e_matrix = (fun _ -> Base.Set.singleton (Base.univ_base ()));
		       one_dup_e_matrix = (fun _ -> Base.Set.singleton (Base.univ_base ()))})
let dup = Dup (2,Some {e_matrix = (fun _ -> Base.Set.empty); 
		       one_dup_e_matrix = (fun _ -> Base.Set.singleton (Base.univ_base ()))})

  
(***********************************************
 * utilities
 ***********************************************)

let rec is_test (t : term) : bool =
  match t with
  | Assg _ -> false
  | Test _ -> true
  | Dup _ -> false
  | Times (_,x,_) -> List.for_all is_test x
  | Plus (_,x,_) -> TermSet.for_all is_test x
  | Not (_,x,_) -> is_test x || failwith "May not negate an action"
  | Star (_,x,_) -> is_test x
  | (Zero _ | One _) -> true

let rec vars_in_term (t : term) : Field.t list =
  match t with
  | (Assg (_,x,_,_) | Test(_,x,_,_)) -> [x]
  | Times (_,x,_) -> List.concat (List.map vars_in_term x)
  | Plus (_,x,_) -> List.concat (List.map vars_in_term (TermSet.elements x))
  | (Not (_,x,_) | Star (_,x,_)) -> vars_in_term x
  | (Dup _ | Zero _ | One _) -> []


(* Collect the possible values of each variable *)
let values_in_term (t : term) : UnivMap.t =
  let rec collect (t : term) (m : UnivMap.t) : UnivMap.t =
  match t with 
  | (Assg (_,x,v,_) | Test (_,x,v,_)) -> UnivMap.add x v m
  | Plus (_,s,_) -> TermSet.fold collect s m
  | Times (_,s,_) -> List.fold_right collect s m
  | (Not (_,x,_) | Star (_,x,_)) -> collect x m
  | (Dup _ | Zero _ | One _) -> m in
  collect t UnivMap.empty



(***********************************************
 * simplify
 ***********************************************)

(* NOTE : any simplification routine must re-set ID to -1*)

(* 
   TODO : at the moment, simplify invalidates the ID of 
   already-simplified terms.  Might want to put an old=new
   check in the code somewhere.
*)

(* flatten terms *)
let flatten_sum o (t : Term.t list) : Term.t =
  let open Term in 
  let f (x : Term.t) = 
    match x with 
      | Plus (_,v,_) -> 
	(TermSet.elements v) 
      | (Zero _) -> [] 
      | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  let t2 = TermSet.of_list t1 in
  match TermSet.elements t2 with 
    | [] -> zero
    | [x] -> x
    | _ ->  (Term.Plus (-1, t2, o))




let get_cache_option t :  'a option = 
  match t with 
    | Assg (_,_,_,c) -> c
    | Test (_,_,_,c) -> c
    | Dup (_,c) -> c
    | Plus (_,_,c) -> c
    | Times (_,_,c) -> c
    | Not (_,_,c) -> c
    | Star (_,_,c) -> c
    | Zero (_,c) -> c
    | One (_,c) -> c

(* smart constructors *)

let get_cache t :  cached_info  = 
  match get_cache_option t with 
    | Some c -> c
    | None -> failwith "can't extract; caache not set"

let e_matrix t = 
  (get_cache t).e_matrix ()

let one_dup_e_matrix t = 
  (get_cache t).one_dup_e_matrix ()
    
(* this is calculating the E matrix, effectively *)
let rec fill_cache t0 = 
  let open Base in 
  let open Base.Set in
  let negate (x : Decide_Util.Field.t) (v : Decide_Util.Value.t) : Base.Set.t =
    Base.Set.singleton(Base.of_neg_test x v) in
  match get_cache_option t0 with 
    | Some _ -> t0
    | None -> 
      begin 
	match t0 with 
	  | One (id,_) -> 
	    One (id,Some {e_matrix = (fun _ -> singleton (univ_base ()));
			  one_dup_e_matrix = (fun _ -> singleton (univ_base ()))})
	  | Zero (id,_) -> 
	    Zero(id,Some {e_matrix = (fun _ -> empty);
			  one_dup_e_matrix = (fun _ -> empty)})
	  | Assg(id,field,v,_) -> 
	    let r = thunkify (fun _ -> singleton (of_assg field v)) in
	    Assg(id,field,v,Some {e_matrix = r; one_dup_e_matrix = r})
	  | Test(id,field,v,_) ->
	    let r = thunkify (fun _ -> singleton (of_test field v)) in
	    Test(id,field,v, Some {e_matrix = r; one_dup_e_matrix = r})
	  | Dup (id,_) -> 
	    Dup(id,Some {e_matrix = (fun _ -> empty); one_dup_e_matrix = (fun _ -> singleton (univ_base ()))})
	  | Plus (id,ts,_) ->
	    let ts = TermSet.map fill_cache ts in
	    let r = thunkify (fun _ -> TermSet.fold 
	      (fun t acc -> union ((get_cache t).e_matrix ()) acc) ts empty ) in 
	    let r_onedup =   thunkify (fun _ -> TermSet.fold 
	      (fun t acc -> union ((get_cache t).one_dup_e_matrix ()) acc) ts empty) in 
	    Plus(id,ts,Some {e_matrix = r; one_dup_e_matrix = r_onedup})
	  | Times (id,tl,_) -> 
	    let tl = List.map fill_cache tl in 
	    let r = thunkify (fun _ -> List.fold_right (fun t acc ->  mult ((get_cache t).e_matrix ()) acc) tl
	      (singleton (univ_base ())) ) in 
	    let r_onedup = thunkify (fun _ -> List.fold_right 
	      (fun t acc ->  mult ((get_cache t).one_dup_e_matrix ()) acc) tl
	      (singleton (univ_base ())) ) in 
	    Times(id,tl,Some { e_matrix = r; one_dup_e_matrix  = r_onedup})
	  | Not (id,x,_) -> 
	    let x = fill_cache x in 
	    let m = thunkify (fun _ -> match x with
	      | Zero _ -> singleton (univ_base ())
	      | One _ -> empty
	      | Test (_,x,v,_) -> negate x v
	      | _ -> failwith "De Morgan law should have been applied") in 
	    Not(id,x,Some {e_matrix = m; one_dup_e_matrix = m})
	  | Star (id,x,_) ->
	    let x = fill_cache x in
	    let get_fixpoint s = 
	      Printf.printf "getting fixpoint...\n%!";
	      let s1 = add (univ_base ()) s in
	      (* repeated squaring completes after n steps, where n is the log(cardinality of universe) *)
	      let rec f cntr s r  =
		if cntr > 1000 then Printf.printf "%u" cntr; 
		if equal s r then (Printf.printf "got fixpoint!\n%!"; s)
		else f (cntr + 1) (mult s s) s in
	      f 0 (mult s1 s1) s1 in 
	    let me = thunkify (fun _ -> get_fixpoint ((get_cache x).e_matrix())) in 
	    let mo = thunkify (fun _ -> get_fixpoint ((get_cache x).one_dup_e_matrix())) in
	    Star(id,x,Some {e_matrix = me; one_dup_e_matrix = mo}) end 

let timeshash : (uid list, Term.t) Hashtbl.t  = Hashtbl.create 100 
let plushash : (uid list, Term.t) Hashtbl.t  = Hashtbl.create 100 
let nothash : (uid, Term.t) Hashtbl.t  = Hashtbl.create 100 
let starhash : (uid, Term.t) Hashtbl.t  = Hashtbl.create 100 
let testhash : (Field.t * Value.t, Term.t) Hashtbl.t  = Hashtbl.create 100 
let assghash : (Field.t * Value.t, Term.t) Hashtbl.t  = Hashtbl.create 100 
let counter = ref 3 

let rec hashcons (t : Term.t) : Term.t = 
  (* unique ID utilities *)
  let cache_exists t = match get_cache_option t with None -> false | Some _ -> true in 

  let ids_from_list tl = 
    List.map extract_uid tl in 
  let ids_from_set ts = 
    TermSet.fold (fun t acc -> (extract_uid t) :: acc) ts [] in 
  let getandinc () = 
    let ret = !counter in 
    if ret > 1073741822
    then failwith "you're gonna overflow the integers, yo";
    counter := !counter + 1; 
    ret in 
  let get_or_make hash k cnstr = 
    try let ret = Hashtbl.find hash k  in 
	Printf.printf "successs: %s\n" (Term.to_string ret); ret
    with Not_found -> 
      let new_id = getandinc () in 
      let ret = fill_cache (cnstr new_id) in 
      Hashtbl.replace hash k ret; 
      Printf.printf "failure: %d\n" new_id;
      ret in 

  (* actual algo *)
  match t with 
    | Dup (-1,None) -> dup
    | One (-1,None) -> one
    | Zero (-1,None) -> zero
    | Assg(-1,f,v,None) -> 
      get_or_make assghash (f,v) (fun id -> Assg(id,f,v,None))
    | Test(-1,f,v,None) -> 
      get_or_make testhash (f,v) (fun id -> Test(id,f,v,None))
    | Plus (-1,ts,None) -> 
      let ts = TermSet.map hashcons ts in 
      let ids = (ids_from_set ts) in 
      get_or_make plushash ids (fun id -> Plus(id,ts,None))
    | Times (-1,tl,None) -> 
      let tl = List.map hashcons tl in
      let ids = ids_from_list tl in 
      get_or_make timeshash ids (fun id -> Times(id,tl,None))
    | Not (-1, t,None) -> 
      let t = hashcons t in 
      get_or_make nothash (extract_uid t) (fun id -> Not(id, t, None))
    | Star (-1, t,None) -> 
      let t = hashcons t in 
      get_or_make starhash (extract_uid t) (fun id -> Star(id,t,None))
    | already_assigned when (extract_uid t) <> -1 && (cache_exists t) -> already_assigned
    | already_assigned when (extract_uid t) <> -1 -> failwith "Cache was invalidated but ID was not!"
    | already_assigned when (cache_exists t) -> failwith "ID was invalidated but cache was not!"
    | _ -> failwith "some other failure occured!" 


let rec simplify (t : Term.t) : Term.t =
  let ret = match t with
    | Plus (_,x,_) -> hashcons (flatten_sum None (List.map simplify 
						    (TermSet.elements x)))
    | Times (_,x,_) -> hashcons (flatten_product None (List.map simplify x))
    | Not (_,x,_) -> hashcons (flatten_not None (simplify x))
    | Star (_,x,_) -> hashcons (flatten_star None (simplify x))
    | _ -> hashcons t  in 
  ret

(* apply De Morgan laws to push negations down to the leaves *)
let deMorgan (t : term) : term =
  let rec dM (t : term) : term =
    let f o x = dM (Not (-1,x,o)) in
    match t with 
      | (Assg _ | Test _ | Zero _ | One _ | Dup _) -> t
      | Star (_, x, o) -> Star (-1, dM x, o)
      | Plus (_,x, o) -> Plus (-1,TermSet.map dM x, o)
      | Times (_,x, o) -> Times (-1,List.map dM x, o)
      | Not (_,(Not (_,x,_)),_) -> dM x
      | Not (_,(Plus (_,s,_)),_) -> Times (-1, List.map (fun e -> (f (None) e)) (TermSet.elements s), None)
      | Not (_,Times (_,s,_),_) -> Plus (-1, TermSet.of_list (List.map (fun e -> f (None) e) s), None)
      | Not (_,Star (_,x,_),_) ->
	if is_test x then zero
	else failwith "May not negate an action"
      | Not (_, (Zero _),_ ) -> one
      | Not (_, (One _),_ ) -> zero
      | Not(_, (Dup _),_) -> failwith "you may not negate a dup!"
      | Not(_, (Assg _),_) -> failwith "you may not negate an assg!"
      | Not (_, (Test _),_) -> t in
  simplify (dM t)


let make_assg  (f,v) = 
  (hashcons  (Assg(-1,f,v,None)))

let make_test (f,v) = 
  (hashcons  (Test(-1,f,v,None)))
    
let make_dup = dup
  
let make_plus ts = 
  (hashcons (flatten_sum None ts))
    
let make_times tl = 
  (hashcons  (flatten_product None tl))
    
let make_star t = 
  (hashcons (flatten_star None t))
    
let make_zero = zero
  
let make_one = one 

let make_not t = 
  (hashcons (deMorgan (flatten_not None t)))

(* these things should go in Util, but depend on the structure of Ast.Term.t *)
      
let hits = ref 0 
let misses = ref 1 

let memoize (f : Term.t -> 'a) = f (*
  let hash = Hashtbl.create 100 in 
  (fun b -> 
    try let ret = Hashtbl.find hash b in
	(hits := !hits + 1;
	 ret)
    with Not_found -> 
      (misses := !misses + 1;
       let ret = f b in 
       Hashtbl.replace hash b ret;
       ret
      )) 
				   *)

(* SPINES *)

open Term

module Formula = struct
  type t = 
    | Eq of Term.t * Term.t
    | Le of Term.t * Term.t
        
  let make_eq (t1:Term.t) (t2:Term.t) : t = 
    Eq (t1,t2)

  let make_le (t1:Term.t) (t2:Term.t) : t = 
    Le (t1,t2)

  let to_string (f:t) : string =
    match f with
      | Eq (s,t) -> 
        Printf.sprintf "%s == %s" 
          (Term.to_string s) (Term.to_string t)
      | Le (s,t) -> 
        Printf.sprintf "%s <= %s" 
          (Term.to_string s) (Term.to_string t)

  let compare (f1:t) (f2:t) : int = 
    match f1,f2 with
      | Eq(s1,t1), Eq(s2,t2) -> 
        let cmp = Term.compare s1 s2 in 
        if cmp <> 0 then cmp 
        else Term.compare t1 t2
      | Le(s1,t1), Le(s2,t2) -> 
        let cmp = Term.compare s1 s2 in 
        if cmp <> 0 then cmp 
        else Term.compare t1 t2
      | Eq _, _ -> -1
      | _ -> 1  

  let equal (f1:t) (f2:t) : bool = 
    compare f1 f2 = 0

  let terms (f:t) = 
    match f with 
      | Eq (s,t) -> (s,t)
      | Le (s,t) -> (s,t)
end

(* START FOSTER *)
open Decide_Util

exception Empty

let utf8 = ref false 

(***********************************************
 * syntax
 ***********************************************)
module rec Term : sig 
  type d = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One 
  and t = 
      { uid : int;
        desc : d;
        hash : int;
        mutable spines : TermPairSet.t option }

  val make_assg : Field.t -> Value.t -> t
  val make_test : Field.t -> Value.t -> t
  val make_dup : unit -> t
  val make_plus : TermSet.t -> t
  val make_times : t list -> t
  val make_not : t -> t
  val make_star : t -> t
  val make_zero : unit -> t
  val make_one : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val lrspines : t -> TermPairSet.t
  val fields : t -> FieldSet.t
  val values : t -> ValueSet.t
end = struct
  type d = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One 
  and t = 
      { uid : int;        
        desc : d; 
        hash : int;
        mutable spines : TermPairSet.t option }

  type this_t = t

  let compare (t1:t) (t2:t) : int = 
    compare t1.uid t2.uid

  let equal (t1:t) (t2:t) = 
    compare t1 t2 = 0

  let hash (t:t) = 
    t.hash

  let fields = assert false
  let values = assert false
  let univ = assert false


  (* Constructors *)
  let uid_cell = ref 0
  let next_uid () = 
    incr uid_cell;
    !uid_cell

  module FVHash = Hashtbl.Make(struct 
    type t = Field.t * Value.t
    let equal = (=)
    let hash = Hashtbl.hash 
  end)
  module THash = Hashtbl.Make(struct 
    type t = this_t
    let equal = equal
    let hash = hash 
  end)
  module TListHash = Hashtbl.Make(struct 
    type t = this_t list
    let rec equal l1 l2 = 
      match l1,l2 with 
        | [],[] -> true
        | h1::t1, h2::t2 -> 
          compare h1 h2 = 0 && equal t1 t2 
        | _ -> false
    let hash = Hashtbl.hash 
  end)
  module TSetHash = Hashtbl.Make(struct 
    type t = TermSet.t
    let rec equal s1 s2 = TermSet.equal s1 s2
    let hash = TermSet.hash 
  end)

  let zero_cell = ref None
  let make_zero () = 
    match !zero_cell with 
      | Some t -> t
      | None -> 
        let u = next_uid () in 
        let d = Zero in 
        let h = Hashtbl.hash d in 
        let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None} in 
        zero_cell := Some t;
        t

  let one_cell = ref None
  let make_one () = 
    match !one_cell with 
      | Some t -> t
      | None -> 
        let u = next_uid () in 
        let d = One in 
        let h = Hashtbl.hash d in 
        let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None} in 
        one_cell := Some t;
        t

  let assg_hash = FVHash.create 101
  let make_assg f v = 
    try FVHash.find assg_hash (f,v)
    with Not_found -> 
      let u = next_uid () in 
      let d = Assg(f,v) in 
      let h = Hashtbl.hash d in 
      let t = 
        { uid = u;
          desc = d;
          hash = h;
	  spines = None} in 
      FVHash.add assg_hash (f,v) t;
      t

  let test_hash = FVHash.create 101
  let make_test f v = 
    try FVHash.find test_hash (f,v)
    with Not_found -> 
      let u = next_uid () in 
      let d = Test(f,v) in 
      let h = Hashtbl.hash d in 
      let t = 
        { uid = u;
          desc = d;
          hash = h;
	  spines = None} in 
      FVHash.add test_hash (f,v) t;
      t
                
  let dup_cell = ref None
  let make_dup () = 
    match !dup_cell with 
      | Some t -> t
      | None -> 
        let u = next_uid () in 
        let d = Dup in 
        let h = Hashtbl.hash d in 
        let t = 
          { uid = u;
            desc = d;
            hash = h; 
	    spines = None} in 
        dup_cell := Some t;
        t


  let plus_hash = TSetHash.create 101
  (* flatten terms *)
  let rec flatten_sum (t : TermSet.t ) : Term.t =
    let f (x : Term.t) = 
      match x.desc with 
	| Term.Plus v -> v
	| Term.Zero -> TermSet.empty
	| _ -> TermSet.singleton x in
    let (t2 : TermSet.t) = TermSet.fold (fun e -> TermSet.union (f e)) t TermSet.empty in 
    match TermSet.elements t2 with 
      | [] -> make_zero ()
      | [x] -> x
      | _ ->  make_plus ~flatten:false t2
  and make_plus ?(flatten = true) (ts : TermSet.t) = 
    try TSetHash.find plus_hash ts 
    with Not_found -> 
      if flatten 
      then flatten_sum ts 
      else 
      let u = next_uid () in 
      let d = Plus(ts) in 
      let h = Hashtbl.hash d in 
      let t = 
        { uid = u;
          desc = d;
          hash = h;
	  spines = None} in 
      TSetHash.add plus_hash ts t;
      t

  let times_hash = TListHash.create 101
    
    
  let rec flatten_product (t : Term.t list) : Term.t =
    let f x = match x.desc with 
      | Times v -> v 
      | One  -> [] 
      | _ -> [x] in
    let t1 = List.concat (List.map f t) in
    if List.exists 
      (fun x -> match x.desc with (Zero )  -> true | _ -> false) t1 
    then make_zero ()
    else match t1 with 
      | [] -> make_one ()
      | [x] -> x 
      | _ ->  make_times ~flatten:false t1
  and make_times ?flatten:(flatten=true) ts = 
    try TListHash.find times_hash ts 
    with Not_found -> 
      if flatten
      then flatten_product ts
      else 
	let u = next_uid () in 
	let d = Times(ts) in 
	let h = Hashtbl.hash d in 
	let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None} in 
	TListHash.add times_hash ts t;
	t
	  
  let is_test _ = assert false

  let star_hash = THash.create 101 

  let rec flatten_star (t : Term.t) : Term.t =
    let t1 = match t.desc with
      | Term.Plus x -> 
	flatten_sum (TermSet.filter (fun s -> not (is_test s)) x)
      | _ -> t in
    if is_test t1 then make_one ()
    else match t1.desc with
      | Star _ -> t1
      | _ -> make_star ~flatten:false t1
  and make_star ?flatten:(flatten=true) t0 = 
    try THash.find star_hash t0
    with Not_found -> 
      if flatten
      then flatten_star t0 
      else 
	let u = next_uid () in 
	let d = Star(t0) in 
	let h = Hashtbl.hash d in 
	let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None} in 
	THash.add star_hash t0 t;
	t

  
  let not_hash = THash.create 101 
(* apply De Morgan laws to push negations down to the leaves *)
  let rec deMorgan (t : Term.t) : Term.t =
    let rec dM (t : Term.t) : Term.t =
      let f x = dM (make_not ~flatten:false x) in
      match t.desc with 
	| (Assg _ | Test _ | Zero  | One  | Dup ) -> t
	| Star (x) -> make_star (dM x)
	| Plus (x) -> make_plus (TermSet.map dM x)
	| Times (x) -> make_times (List.map dM x)
	| Not {desc = Not x} -> dM x
	| Not {desc = Plus s} -> make_times (List.map f (TermSet.elements s))
	| Not {desc = Times s} -> make_plus (TermSet.of_list (List.map f s))
	| Not {desc = Star x} ->
	  if is_test x then make_zero ()
	  else failwith "May not negate an action"
	| Not {desc = Zero} -> make_one ()
	| Not {desc = One} -> make_zero ()
	| Not {desc = Dup} -> failwith "you may not negate a dup!"
	| Not {desc = Assg _} -> failwith "you may not negate an assg!"
	| Not {desc = Test _} -> t in
    dM t
  and flatten_not (t : Term.t) : Term.t =
    match t.desc with
      | Not y -> y
      | Zero -> make_one ()
      | One ->  make_zero ()
      | _ -> make_not ~flatten:false t
  and make_not ?flatten:(flatten=true) t0 = 
    try THash.find not_hash t0
    with Not_found -> 
      if flatten 
      then deMorgan (flatten_not t0)
      else
	let u = next_uid () in 
	let d = Not(t0) in 
	let h = Hashtbl.hash d in 
	let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None} in 
	THash.add not_hash t0 t;
	t

  let make_plus = make_plus ~flatten:true
  let make_times = make_times ~flatten:true
  let make_star = make_star ~flatten:true
  let make_not = make_not ~flatten:true

  let rec to_string (t : t) : string =
    let out_precedence (t : t) : int =
      match t.desc with
        | Plus _ -> 0
        | Times _ -> 1
        | Not _ -> 2
        | Star _ -> 3
        | _ -> 4 in
    let protect (u:t) : string =
      let s = to_string u in
      if out_precedence t <= out_precedence u then s
      else Printf.sprintf "(%s)" s in 
    let assoc_to_string (op : string) (init : string) (s : string list) : string = 
      match s with
        | [] -> init
        | _ -> String.concat op s in
    match t.desc with
      | Assg (f,v) -> 
        Printf.sprintf "%s:=%s" 
          (Field.to_string f) (Value.to_string v)
      | Test (f,v) -> 
        Printf.sprintf "%s=%s" 
          (Field.to_string f) (Value.to_string v)
      | Dup -> 
        "dup"
      | Plus (ts) -> 
        assoc_to_string " + " "0" 
          (List.map protect (TermSet.elements ts))
      | Times (ts) -> 
        assoc_to_string ";" "1" (List.map protect ts)
      | Not (t) -> 
        (if !utf8 then "¬" else "~") ^ (protect t)
      | Star (t) -> 
        (protect t) ^ "*"
      | Zero -> 
        "drop"
      | One -> 
        "id"


  (* Operations *)
  let rspines (t0 : Term.t) : TermSet.t =
    let rec sp (t0 : Term.t) : TermSet.t =
      match t0.desc with
	| Dup -> 
          TermSet.singleton (make_one ())
	| Plus (ts) -> 
          TermSet.bind ts sp
	| Times ([]) -> TermSet.empty
        | Times ([t]) -> sp t
        | Times (th::tt) -> 
          let u = sp th in
          let v = sp (make_times tt) in
          let s = TermSet.map (fun x -> make_times (x :: tt)) u in
          TermSet.union s v
	| Star (t) ->
          let s = sp t in
          TermSet.map (fun x -> make_times [x; t0]) s
	| (Assg _ | Test _ | Not _ | Zero | One) -> 
          TermSet.empty in
    sp t0
      
  let rec lrspines (t0 : Term.t) : TermPairSet.t =
    match t0.desc with
      | Dup -> 
        TermPairSet.singleton (make_one (), make_one ())
      | Times ([]) -> 
        TermPairSet.empty
      | Times([t]) -> 
        lrspines t
      | Times(th::tt) -> 
	let u = lrspines th in
	let v = lrspines (make_times tt) in
	let f (l,r) = (l, make_times (r :: tt)) in
	let r = TermPairSet.map f u in
	let g (l,r) = (make_times [th;l],r) in
	let s = TermPairSet.map g v in
        TermPairSet.union r s
      | Star (t) ->
        let s = lrspines t in
        let f (l,r) = (make_times [t0;l], make_times [r;t0]) in 
        TermPairSet.map f s
      | Plus (ts) -> 
        TermSet.fold (fun x t -> TermPairSet.union (lrspines x) t) ts TermPairSet.empty
      | (Assg _ | Test _ | Not _ | Zero | One) -> 
        TermPairSet.empty      

end and TermMap : sig 
  include Map.S with type key = Term.t
end  = Map.Make(struct 
  type t = Term.t
  let compare = Term.compare
end
)

and TermPairSet : sig 
  include Set.S with type elt = Term.t * Term.t
  val map : (elt -> elt) -> t -> t
  val bind : t -> (elt -> t) -> t
end = struct 
  include Set.Make(struct 
    type t = Term.t * Term.t
    let compare (al,ar) (bl,br) = 
      match Term.compare al bl with 
	| 0 -> Term.compare ar br
	| o -> o 
  end)
  let map f ts = 
    fold (fun (l,r) acc -> add (f (l,r)) acc) ts empty
  let bind ts f = 
    fold (fun (l,r) t -> union (f (l,r)) t) ts empty

end and TermSet : sig 
  include Set.S with type elt = Term.t
  val map : (elt -> elt) -> t -> t
  val bind : t -> (elt -> t) -> t
  val hash : t -> int
  val to_string : t -> string
  val of_list : Term.t list -> t
end = struct
  include Set.Make(struct
    type t = Term.t

    let compare (ts1:t) (ts2:t) : int = 
      Term.compare ts1 ts2
  end)

  let map (f : elt -> elt) (ts : t) : t =
    fold (fun x -> add (f x)) ts empty

  let bind (ts : t) (f : elt -> t) : t =
    fold (fun x t -> union (f x) t) ts empty

  let hash (ts:t) : int = 
    Hashtbl.hash ts

  let to_string (ts:t) : string = 
    fold 
      (fun t acc -> 
        Printf.sprintf "%s%s%s" 
          acc (if acc = "" then "" else "\n")
          (Term.to_string t))
      ts "" 

  let of_list (ts:elt list) : t = 
    List.fold_left (fun acc t -> add t acc) empty ts 
end 
