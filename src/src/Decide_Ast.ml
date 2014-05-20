open Decide_Util

exception Empty

let utf8 = ref false 

(***********************************************
 * syntax
 ***********************************************)

let biggest_int = ref 0  
     
  module rec Term : sig      
    type uid = int
    type t =
      | Assg of uid * Field.t * Value.t
      | Test of uid * Field.t * Value.t
      | Dup of uid 
      | Plus of uid * TermSet.t
      | Times of uid * t list
      | Not of uid * t
      | Star of uid * t
      | Zero of uid
      | One of uid

  (* pretty printers + serializers *)
  val to_string : t -> string
  val to_string_sexpr : t -> string
  val int_of_uid : uid -> int

  (* utilities for Map, Hashtbl, etc *)
  val old_compare : (t -> t -> int) ref
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool


  (* because module troubles *)
  val ts_elements : (TermSet.t -> t list) ref
  val ts_map : ((t -> t) -> TermSet.t -> TermSet.t) ref
  val extract_uid : t -> uid
  val default_uid : uid

  end = struct 
    type uid = int	

    let default_uid = -1

    type t =
      | Assg of uid * Field.t * Value.t
      | Test of uid * Field.t * Value.t
      | Dup of uid 
      | Plus of uid * TermSet.t
      | Times of uid * t list
      | Not of uid * t
      | Star of uid * t
      | Zero of uid
      | One of uid

    let extract_uid = 
      function 
	| Assg (id,_,_)
	| Test (id,_,_)
	| Dup id 
	| Plus (id,_)
	| Times (id,_)
	| Not (id,_)
	| Star (id,_)
	| Zero (id)
	| One (id)
	  -> id 

    let ts_elements : (TermSet.t -> t list) ref  = 
      ref (fun _ -> failwith "module issues")
      
    let ts_map : ((t -> t) -> TermSet.t -> TermSet.t) ref = 
      ref (fun _ _ -> failwith "module issues")
  
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
	| Assg (_, var, value) -> Printf.sprintf "%s:=%s" 
	  (Field.to_string var) (Value.to_string value)
	| Test (_, var, value) -> Printf.sprintf "%s=%s" 
	  (Field.to_string var) (Value.to_string value)
	| Dup _ -> "dup"
	| Plus (_,x) -> assoc_to_string " + " "0" (List.map protect 
						     ( !ts_elements x ))
	| Times (_,x) -> assoc_to_string ";" "1" (List.map protect x)
	| Not (_,x) -> (if !utf8 then "¬" else "~") ^ (protect x)
	| Star (_,x) -> (protect x) ^ "*"
	| Zero _ -> "drop"
	| One _ -> "pass"


  let rec to_string_sexpr = function 
    | Assg ( _, var, value) -> Printf.sprintf "(%s:=%s)"
      (Field.to_string var) (Value.to_string value)
    | Test ( _, var, value) -> Printf.sprintf "(%s=%s)"
      (Field.to_string var) (Value.to_string value)
    | Dup _ -> "dup"
    | Plus (_,x) -> 
      Printf.sprintf "(+ %s)" 
	(List.fold_right 
	   (fun x -> Printf.sprintf "%s %s" (to_string_sexpr x)) 
	   (!ts_elements x) "")
    | Times (_, x) -> 
      Printf.sprintf "(; %s)" 
	(List.fold_right 
	   (Printf.sprintf "%s %s") (List.map to_string_sexpr x) "")
    | Not (_, x) -> (if !utf8 then "¬" else "~") ^ 
      (Printf.sprintf "(%s)" (to_string_sexpr x))
    | Star (_, x) -> (Printf.sprintf "(%s)" (to_string_sexpr x)) ^ "*"
    | Zero _ -> "drop"
    | One _ -> "pass"


    let int_of_uid x = x

    let old_compare : (t -> t -> int) ref = ref (fun _ _ -> failwith "dummy")

    let compare a b = 
      match extract_uid a, extract_uid b with 
	| (-1,_ | _,-1) -> 
	  !old_compare a b
	| uida,uidb -> 
	  let myres = Pervasives.compare uida uidb in 
	  if Decide_Util.debug_mode 
	  then 
	    match myres, !old_compare a b
	    with 
	      | 0,0 -> 0 
	      | 0,_ -> 
		Printf.printf "about to fail: Terms %s and %s had uid %u\n"
		  (to_string a) (to_string b) (extract_uid a);
		failwith "new said equal, old said not"
	      | _,0 -> 
		Printf.printf "about to fail: Terms %s and %s had uid %u\n"
		  (to_string a) (to_string b) (extract_uid a);
		failwith "old said equal, new said not"
	      | a,_ -> a
	  else myres
    let equal a b = 
      (compare a b) = 0

    let rec invalidate_id = function 
      | Assg (_,l,r) -> Assg(-1,l,r)
      | Test (_,l,r) -> Test(-1,l,r)
      | Plus (_,es) -> Plus(-1,!ts_map invalidate_id es)
      | Times (_,es) -> Times(-1,List.map invalidate_id es)
      | Not (_,e) -> Not(-1,invalidate_id e)
      | Star (_,e) -> Star(-1,invalidate_id e)
      | other -> other

	
    let hash a = match extract_uid a with 
      | -1 -> Hashtbl.hash (invalidate_id a)
      | a -> Hashtbl.hash a


  end

  and TermSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
  val from_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val return : elt -> t
end with type elt = Term.t = struct
  include Set.Make (Term)
  let map (f : elt -> elt) (ts : t) : t =
    fold (fun x -> add (f x)) ts empty
  let from_list (tl : elt list) : t =
    List.fold_right add tl empty
  let bind (ts : t) (f : elt -> t) : t =
    fold (fun x t -> union (f x) t) ts empty
  let return = singleton
end  
  
let _ = Term.ts_elements := TermSet.elements
let _ = Term.ts_map := TermSet.map

let _ = Term.old_compare := 
  (let rec oldcomp = (fun e1 e2 -> 
    let open Term in 
    match e1,e2 with 
      | (Plus (_,al)),(Plus (_,bl)) -> TermSet.compare al bl
      | (Times (_,al)),(Times (_,bl)) -> 
	(match List.length al, List.length bl with 
	  | a,b when a = b -> 
	    List.fold_right2 (fun x y acc -> 
	      if (acc = 0) 
	      then oldcomp x y 
	      else acc
	    ) al bl 0
	  | a,b when a < b -> -1 
	  | a,b when a > b -> 1
	  | _ -> failwith "stupid Ocaml compiler thinks my style is bad"
	)
      | ((Not (_,t1),Not (_,t2)) | (Star (_,t1),Star (_,t2))) -> 
	oldcomp t1 t2
      | _ -> Pervasives.compare e1 e2)
   in oldcomp )
    
open Term
type term = Term.t

module UnivMap = Decide_Util.SetMapF (Field) (Value)
 
type formula = Eq of Term.t * Term.t
	       | Le of Term.t * Term.t

let zero = Zero 0
let one = One 1
let dup = Dup 2


(***********************************************
 * output
 ***********************************************)


let term_to_string = Term.to_string

let formula_to_string (e : formula) : string =
  match e with
  | Eq (s,t) -> Term.to_string ( s) ^ " == " ^ 
    Term.to_string ( t)
  | Le (s,t) -> Term.to_string ( s) ^ " <= " ^ 
    Term.to_string ( t)

let termset_to_string (ts : TermSet.t) : string =
  let l = TermSet.elements ts in
  let m = List.map term_to_string l in
  String.concat "\n" m

  
(***********************************************
 * utilities
 ***********************************************)

let terms_in_formula (f : formula) =
  match f with (Eq (s,t) | Le (s,t)) -> s,t

let rec is_test (t : term) : bool =
  match t with
  | Assg _ -> false
  | Test _ -> true
  | Dup _ -> false
  | Times (_,x) -> List.for_all is_test x
  | Plus (_,x) -> TermSet.for_all is_test x
  | Not (_,x) -> is_test x || failwith "May not negate an action"
  | Star (_,x) -> is_test x
  | (Zero _ | One _) -> true

let rec vars_in_term (t : term) : Field.t list =
  match t with
  | (Assg (_,x,_) | Test(_,x,_)) -> [x]
  | Times (_,x) -> List.concat (List.map vars_in_term x)
  | Plus (_,x) -> List.concat (List.map vars_in_term (TermSet.elements x))
  | (Not (_,x) | Star (_,x)) -> vars_in_term x
  | (Dup _ | Zero _ | One _) -> []


(* Collect the possible values of each variable *)
let values_in_term (t : term) : UnivMap.t =
  let rec collect (t : term) (m : UnivMap.t) : UnivMap.t =
  match t with 
  | (Assg (_,x,v) | Test (_,x,v)) -> UnivMap.add x v m
  | Plus (_,s) -> TermSet.fold collect s m
  | Times (_,s) -> List.fold_right collect s m
  | (Not (_,x) | Star (_,x)) -> collect x m
  | (Dup _ | Zero _ | One _) -> m in
  collect t UnivMap.empty

let rec contains_a_neg term = 
  match term with 
    | (Assg _ | Test _ | Dup _ | Zero _ | One _) -> false
    | Not _ -> true
    | Times (_,x) -> List.fold_left 
      (fun acc x -> acc || (contains_a_neg x)) false x
    | Plus (_,x) -> TermSet.fold 
      (fun x acc -> acc || (contains_a_neg x)) x false 
    | Star (_,x) -> contains_a_neg x



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
let flatten_sum (t : Term.t list) : Term.t =
  let open Term in 
  let f (x : Term.t) = 
    match x with 
      | Plus (_,v) -> 
	(TermSet.elements v) 
      | (Zero _) -> [] 
      | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  let t2 = TermSet.from_list t1 in
  match TermSet.elements t2 with 
    | [] -> zero
    | [x] -> x
    | _ ->  (Term.Plus (-1, t2))
    
let flatten_product (t : Term.t list) : Term.t =
  let f x = match x with 
    | Times (_,v) -> v 
    | One _  -> [] 
    | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  if List.exists 
    (fun x -> match x with (Zero _)  -> true | _ -> false) t1 
  then zero
  else match t1 with 
    | [] -> one
    | [x] -> x 
    | _ ->  (Term.Times (-1,t1))
    
let flatten_not (t : Term.t) : Term.t =
  match t with
  | Not (_,y) -> y
  | Zero _ -> one
  | One _ ->  zero
  | _ -> (Term.Not (-1,t))

let flatten_star (t : Term.t) : Term.t =
  let t1 = match t with
  | Plus (_,x) -> 
    flatten_sum (List.filter (fun s -> not (is_test s))
		   (TermSet.elements x))
  | _ -> t in
  if is_test t1 then one
  else match t1 with
  | Star _ -> t1
  | _ -> (Term.Star (-1,t1))
    
let rec simplify (t : Term.t) : Term.t =
  match t with
  | Plus (_,x) -> flatten_sum (List.map simplify 
			       (TermSet.elements x))
  | Times (_,x) -> flatten_product (List.map simplify x)
  | Not (_,x) -> flatten_not (simplify x)
  | Star (_,x) -> flatten_star (simplify x)
  | _ -> t

    
let contains_dups (t : term) : bool =
  let rec contains t =
    match t with 
    | (Assg _ | Test _ | Zero _ | One _) -> false
    | Dup _ -> true
    | Plus (_,x) ->  
      (TermSet.fold (fun e acc ->  (contains e) || acc)  x false)
    | Times (_,x) ->  
      (List.fold_left (fun acc e -> (contains e) || acc) false x)
    | Not (_,x) ->  (contains x)
    | Star (_,x) ->  (contains x) in
  contains t


(* apply De Morgan laws to push negations down to the leaves *)
let deMorgan (t : term) : term =
  let rec dM (t : term) : term =
    let f x = dM (Not (-1,x)) in
    match t with 
      | (Assg _ | Test _ | Zero _ | One _ | Dup _) -> t
      | Star (_, x) -> Star (-1, dM x)
      | Plus (_,x) -> Plus (-1,TermSet.map dM x)
      | Times (_,x) -> Times (-1,List.map dM x)
      | Not (_,(Not (_,x))) -> dM x
      | Not (_,(Plus (_,s))) -> Times (-1, List.map f (TermSet.elements s))
      | Not (_,Times (_,s)) -> Plus (-1, TermSet.from_list (List.map f s))
      | Not (_,Star (_,x)) ->
	if is_test x then zero
      else failwith "May not negate an action"
      | Not (_, (Zero _)) -> one
      | Not (_, (One _)) -> zero
      | Not(_, (Dup _)) -> failwith "you may not negate a dup!"
      | Not(_, (Assg _)) -> failwith "you may not negate an assg!"
      | Not (_, (Test _)) -> t in
  simplify (dM t)


(* smart constructors *)

let assign_ids = 
  let timeshash = Hashtbl.create 100 in 
  let plushash = Hashtbl.create 100 in 
  let nothash = Hashtbl.create 100 in 
  let starhash = Hashtbl.create 100 in 
  let testhash = Hashtbl.create 100 in 
  let assghash = Hashtbl.create 100 in 
  let extract_uid t = 
    match extract_uid t with 
      | -1 -> failwith "BAD! ASSIGNING BASED ON -1 UID"
      | i -> i in 
  let ids_from_list tl = 
    List.map extract_uid tl in
  let ids_from_set ts = 
    TermSet.fold (fun t acc -> (extract_uid t) :: acc) ts [] in
  let counter = ref 3 in 
  let getandinc _ = 
    let ret = !counter in 
    if ret > 1073741822
    then failwith "you're gonna overflow the integers, yo";
    counter := !counter + 1; 
    ret in 
  let rec assign_ids t = 
    match t with 
      | Dup _ -> dup
      | One _ -> one 
      | Zero _ -> zero 
      | Assg(-1,f,v) -> 
	(try Hashtbl.find assghash (f,v)
	with Not_found -> 
	  let ret = Assg(getandinc (), f,v) in 
	  Hashtbl.replace assghash (f,v) ret; 
	  ret)
      | Test(-1,f,v) -> 
	(try Hashtbl.find testhash (f,v)
	 with Not_found -> 
	   let ret = Test(getandinc (), f,v) in 
	   Hashtbl.replace testhash (f,v) ret; 
	   ret)
      | Plus (-1,ts) -> 
	let ts = TermSet.map assign_ids ts in 
	let ids = (ids_from_set ts) in 
	(try Hashtbl.find plushash ids 
	 with Not_found -> 
	   let ret = Plus(getandinc (), ts) in 
	   Hashtbl.replace plushash ids ret;
	   ret
	)
      | Times (-1,tl) -> 
	let tl = List.map assign_ids tl in
	let ids = ids_from_list tl in 
	(try Hashtbl.find timeshash ids 
	 with Not_found -> 
	   let ret = Times(getandinc (), tl) in 
	   Hashtbl.replace timeshash ids ret;
	   ret
	)
      | Not (-1, t) -> 
	let t = assign_ids t in 
	(try Hashtbl.find nothash (extract_uid t)
	 with Not_found -> 
	  let ret = Not(getandinc (), t) in 
	  Hashtbl.replace nothash (extract_uid t) ret;
	  ret)
      | Star (-1, t) -> 
	let t = assign_ids t in 
	(try Hashtbl.find starhash (extract_uid t)
	with Not_found -> 
	  let ret = Star(getandinc (), t) in 
	  Hashtbl.replace starhash (extract_uid t) ret;
	  ret)
      | already_assigned -> already_assigned
	  
  in
  assign_ids

  let make_assg (f,v) = 
    assign_ids (Assg(-1,f,v)) 

  let make_test (f,v) = 
    assign_ids (Test(-1,f,v)) 
      
  let make_dup = dup 

  let make_plus ts = 
    assign_ids (flatten_sum ts)

  let make_times tl = 
    assign_ids (flatten_product tl)

  let make_not t = 
    assign_ids (deMorgan (flatten_not t))
      
  let make_star t = 
    assign_ids (flatten_star t)
      
  let make_zero = zero 
    
  let make_one  = one 

  let parse_and_simplify (parse : string -> formula) (s : string) : formula = 
    match parse s with 
      | Eq (l,r) -> Eq(assign_ids (deMorgan (simplify l)), assign_ids (deMorgan (simplify r)))
      | Le (l,r) -> Le(assign_ids (deMorgan (simplify l)), assign_ids (deMorgan (simplify r)))
	


let hits = ref 0 
let misses = ref 1 
module TermHash = Hashtbl.Make(Term)
module TermMap = Map.Make(Term)

let memoize (f : Term.t -> 'a) =
  let hash_version = 
    let hash = TermHash.create 100 in 
    (fun b -> 
      try let ret = TermHash.find hash b in
	  (hits := !hits + 1;
	   ret)
      with Not_found -> 
	(misses := !misses + 1;
	 let ret = f b in 
	 TermHash.replace hash b ret;
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
    let hash = ref TermMap.empty in 
    (fun a b -> 
      try let ret = TermMap.find b !hash in
	  (hits := !hits + 1;
	   ret)
      with Not_found -> 
	(misses := !misses + 1;
	 let ret = f a b in 
	 hash := TermMap.add b ret !hash;
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



(* set dups to 0 *)
let zero_dups (t : term) : term =
  let zval = zero in
  let rec zero (t : term) =
    match t with 
      | (Assg _ | Test _ | Zero _ | One _ ) -> t
      | Dup _ -> zval
      | Plus (_,x) -> Plus (-1, TermSet.map zero x)
      | Times (_,x) -> Times (-1, List.map zero x)
      | Not (_,x) -> Not (-1, zero x)
      | Star (_,x) -> Star (-1, zero x) in
  let zero = memoize zero in 
  assign_ids (simplify (zero t))

(* set dups to 1 *)
let one_dups (t : term) : term =
  let oval = one in 
  let rec one t =
    match t with 
      | (Assg _ | Test _ | Zero _ | One _) -> t
      | Dup _ -> oval
      | Plus (_,x) -> Plus (-1, TermSet.map one x)
      | Times (_,x) -> Times (-1, List.map one x)
      | Not (_,x) -> Not (-1, one x)
      | Star (_,x) -> Star (-1, one x) in
  let one = memoize one in
  assign_ids (simplify (one t))
    
