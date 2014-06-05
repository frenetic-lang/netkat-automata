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
  val map : (elt -> elt) -> t -> t
  val of_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val to_string : t -> string 
  val ts : (t -> string) ref
end with type elt = Term.t = struct
  include Set.Make (Term)
  let map (f : elt -> elt) (ts : t) : t =
    fold (fun x -> add (f x)) ts empty
  let of_list (tl : elt list) : t =
    List.fold_right add tl empty
  let bind (ts : t) (f : elt -> t) : t =
    fold (fun x t -> union (f x) t) ts empty
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
  val ts_map : ((t -> t ) -> TermSet.t -> TermSet.t) ref 
  val ts_elements : (TermSet.t -> t list) ref 
    
end = struct 

  let ts_map = ref (fun _ _ -> failwith "foo") 
  let ts_elements = ref (fun _ -> failwith "foo")
    
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
    match t with
      | Assg (_, var, value,_) -> Printf.sprintf "%s:=%s" 
	(Field.to_string var) (Value.to_string value)
      | Test (_, var, value,_) -> Printf.sprintf "%s=%s" 
	(Field.to_string var) (Value.to_string value)
      | Dup _ -> "dup"
      | Plus (_,x,_) -> assoc_to_string " + " "0" (List.map protect 
						     ( !ts_elements x ))
      | Times (_,x,_) -> assoc_to_string ";" "1" (List.map protect x)
      | Not (_,x,_) -> (if !utf8 then "¬" else "~") ^ (protect x)
      | Star (_,x,_) -> (protect x) ^ "*"
      | Zero _ -> "drop"
      | One _ -> "pass"
	  
	  
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
	Pervasives.compare uida uidb
  let equal a b = 
    (compare a b) = 0
    
  let rec invalidate_id = function 
    | Assg (_,l,r,o) -> Assg(-1,l,r,o)
    | Test (_,l,r,o) -> Test(-1,l,r,o)
    | Plus (_,es,o) -> Plus(-1,!ts_map invalidate_id es,o)
    | Times (_,es,o) -> Times(-1,List.map invalidate_id es,o)
    | Not (_,e,o) -> Not(-1,invalidate_id e,o)
    | Star (_,e,o) -> Star(-1,invalidate_id e,o)
    | other -> other
      
      
  let hash a = match extract_uid a with 
    | -1 -> Hashtbl.hash (invalidate_id a)
    | a -> Hashtbl.hash a
      
end 

    let _ = Term.ts_map := TermSet.map
    let _ = Term.ts_elements := TermSet.elements
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
    
let flatten_product o (t : Term.t list) : Term.t =
  let f x = match x with 
    | Times (_,v,_) -> v 
    | One _  -> [] 
    | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  if List.exists 
    (fun x -> match x with (Zero _)  -> true | _ -> false) t1 
  then zero
  else match t1 with 
    | [] -> one
    | [x] -> x 
    | _ ->  (Term.Times (-1,t1,o))
    
let flatten_not o (t : Term.t) : Term.t =
  match t with
  | Not (_,y,_) -> y
  | Zero _ -> one
  | One _ ->  zero
  | _ -> (Term.Not (-1,t,o))


let flatten_star o (t : Term.t) : Term.t =
  let t1 = match t with
  | Plus (_,x,_) -> 
    flatten_sum None (List.filter (fun s -> not (is_test s))
		   (TermSet.elements x))
  | _ -> t in
  if is_test t1 then one
  else match t1 with
  | Star _ -> t1
  | _ -> (Term.Star (-1,t1,o))



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


let rec hashcons (t : Term.t) : Term.t = 
  (* unique ID utilities *)
  let cache_exists t = match get_cache_option t with None -> false | Some _ -> true in 
  let timeshash : (uid list, Term.t) Hashtbl.t  = Hashtbl.create 100 in 
  let plushash : (uid list, Term.t) Hashtbl.t  = Hashtbl.create 100 in 
  let nothash : (uid, Term.t) Hashtbl.t  = Hashtbl.create 100 in 
  let starhash : (uid, Term.t) Hashtbl.t  = Hashtbl.create 100 in 
  let testhash : (Field.t * Value.t, Term.t) Hashtbl.t  = Hashtbl.create 100 in
  let assghash : (Field.t * Value.t, Term.t) Hashtbl.t  = Hashtbl.create 100 in 
  let counter = ref 3 in
  let ids_from_list tl = 
    List.map extract_uid tl in 
  let ids_from_set ts = 
    TermSet.fold (fun t acc -> (extract_uid t) :: acc) ts [] in 
  let getandinc _ = 
    let ret = !counter in 
    if ret > 1073741822
    then failwith "you're gonna overflow the integers, yo";
    counter := !counter + 1; 
    ret in 
  let get_or_make hash k cnstr = 
    try Hashtbl.find hash k 
    with Not_found -> 
      let new_id = getandinc () in 
      let ret = fill_cache (cnstr new_id) in 
      Hashtbl.replace hash k ret; 
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
  (hashcons  (flatten_sum None ts))
    
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

let memoize (f : Term.t -> 'a) =
  let hash_version = 
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
	)) in 
  if debug_mode 
  then (fun x -> 
    let hv = hash_version x in 
    let fv = f x in 
    (try 
      assert (hv = fv);
    with Invalid_argument _ -> 
      Printf.printf "%s" ("warning: memoize assert could not run:" ^
			     "Invalid argument exception!\n"));
    hv)
  else hash_version

let memoize_on_arg2 f =
  let hash_version = 
    let hash = ref (BatMap.PMap.create Term.compare) in 
    (fun a b -> 
      try let ret = BatMap.PMap.find b !hash in
	  (hits := !hits + 1;
	   ret)
      with Not_found -> 
	(misses := !misses + 1;
	 let ret = f a b in 
	 hash := BatMap.PMap.add b ret !hash;
	 ret
	)) in
  if debug_mode
  then (fun x y -> 
    let hv = hash_version x y in 
    let fv = f x y in
    (try 
      assert (hv = fv);
    with Invalid_argument _ -> 
      Printf.printf "%s" ("warning: memoize assert could not run:" 
			  ^"Invalid argument exception!\n"));
    hv)
  else hash_version


(* SPINES *)

open Term

let rspines (e : term) : TermSet.t =
  let rec sp (e : term) : TermSet.t =
      match e with
	| Dup _ -> TermSet.singleton make_one
	| Plus (_,ts,_) -> TermSet.bind ts sp
	| Times (_,l,_) ->
        (match l with
          | [] -> TermSet.empty
          | [d] -> sp d
          | d :: t ->
            let u = sp d in
            let v = sp (make_times t) in
            let s = TermSet.map (fun x -> make_times (x :: t)) u in
          TermSet.union s v)
	| Star (_,d,_) ->
          let s = sp d in
        TermSet.map (fun x -> make_times [x; e]) s
	| (Assg _ | Test _ | Not _ | Zero _ | One _) -> TermSet.empty in
  TermSet.map simplify (sp e)


  
let rec lrspines (e : term) =
  match e with
    | Dup _ -> TermPairSet.singleton (make_one, make_one)
    | Times (_,l,_) ->
    (match l with
      | [] -> TermPairSet.empty
      | [d] -> lrspines d
      | d :: t ->
	let u = lrspines d in
	let v = lrspines (make_times t) in
	let f (l,r) = 
	  l, simplify (make_times (r :: t)) in
	let r = TermPairSet.map f u in
	let g (l,r) = (simplify (make_times [d;l]),r) in
	let s = TermPairSet.map g v in
      TermPairSet.union r s)
    | Star (_,d,_) ->
      let s = lrspines d in
      let f (l,r) = simplify (make_times [e;l]),
	simplify (make_times [r;e]) in
      TermPairSet.map f s
    | Plus (_,ts,_) -> 
      TermSet.fold (fun x t -> TermPairSet.union (lrspines x) t) ts TermPairSet.empty
    | (Assg _ | Test _ | Not _ | Zero _ | One _) -> TermPairSet.empty      
      
  (* get all lrspines of e and all lrspines of rspines of e *)
  let allLRspines (e :  term) : TermPairSet.t TermMap.t =
    Printf.printf "getting all spines of: %s\n" (Term.to_string e);
   
    let allLR = TermSet.add e (rspines e) in
    let h = ref TermMap.empty in
    let f d = h := TermMap.add d (lrspines d) !h in
    TermSet.iter f allLR; !h
