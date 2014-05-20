open Decide_Util

exception Empty

let utf8 = ref false 

(***********************************************
 * syntax
 ***********************************************)

let biggest_int = ref 0  
     
  module Term : sig      
    type uid = int

    type 'a t =
      | Assg of uid * Decide_Util.Field.t * Decide_Util.Value.t * 'a
      | Test of uid * Decide_Util.Field.t * Decide_Util.Value.t * 'a
      | Dup of uid *'a 
      | Plus of uid * ('a t) BatSet.PSet.t * 'a
      | Times of uid * 'a t list * 'a
      | Not of uid * 'a t *'a 
      | Star of uid * 'a t * 'a
      | Zero of uid * 'a
      | One of uid * 'a 

  (* pretty printers + serializers *)
  val to_string : 'a t -> string
  val to_string_sexpr : 'a t -> string
  val int_of_uid : uid -> int

  (* utilities for Map, Hashtbl, etc *)
  val old_compare : ('a t -> 'a t -> int)
  val compare : 'a t -> 'a t -> int
  val hash : 'a t -> int
  val equal : 'a t -> 'a t -> bool


  (* because module troubles *)
  val extract_uid : 'a t -> uid
  val default_uid : uid

  end = struct 
    type uid = int	

    type 'a t =
      | Assg of uid * Decide_Util.Field.t * Decide_Util.Value.t * 'a
      | Test of uid * Decide_Util.Field.t * Decide_Util.Value.t * 'a
      | Dup of uid *'a 
      | Plus of uid * ('a t) BatSet.PSet.t * 'a
      | Times of uid * 'a t list * 'a
      | Not of uid * 'a t *'a 
      | Star of uid * 'a t * 'a
      | Zero of uid * 'a
      | One of uid * 'a 

    let default_uid = -1

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
  
    let rec to_string (t : 'a t) : string =
      (* higher precedence binds tighter *)
      let out_precedence (t : 'a t) : int =
	match t with
	  | Plus _ -> 0
	  | Times _ -> 1
	  | Not _ -> 2
	  | Star _ -> 3
	  | _ -> 4 (* assignments and primitive tests *) in
      (* parenthesize as dictated by surrounding precedence *)
      let protect (x : 'a t) : string =
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
						     ( BatSet.PSet.elements x ))
	| Times (_,x,_) -> assoc_to_string ";" "1" (List.map protect x)
	| Not (_,x,_) -> (if !utf8 then "¬" else "~") ^ (protect x)
	| Star (_,x,_) -> (protect x) ^ "*"
	| Zero _ -> "drop"
	| One _ -> "pass"


  let rec to_string_sexpr = function 
    | Assg ( _, var, value,_) -> Printf.sprintf "(%s:=%s)"
      (Field.to_string var) (Value.to_string value)
    | Test ( _, var, value,_) -> Printf.sprintf "(%s=%s)"
      (Field.to_string var) (Value.to_string value)
    | Dup _ -> "dup"
    | Plus (_,x,_) -> 
      Printf.sprintf "(+ %s)" 
	(List.fold_right 
	   (fun x -> Printf.sprintf "%s %s" (to_string_sexpr x)) 
	   (BatSet.PSet.elements x) "")
    | Times (_, x,_) -> 
      Printf.sprintf "(; %s)" 
	(List.fold_right 
	   (Printf.sprintf "%s %s") (List.map to_string_sexpr x) "")
    | Not (_, x,_) -> (if !utf8 then "¬" else "~") ^ 
      (Printf.sprintf "(%s)" (to_string_sexpr x))
    | Star (_, x,_) -> (Printf.sprintf "(%s)" (to_string_sexpr x)) ^ "*"
    | Zero _ -> "drop"
    | One _ -> "pass"


    let int_of_uid x = x

    let old_compare =   
      (let rec oldcomp = (fun e1 e2 -> 
	match e1,e2 with 
	  | (Plus (_,al,_)),(Plus (_,bl,_)) -> BatSet.PSet.compare al bl
	  | (Times (_,al,_)),(Times (_,bl,_)) -> 
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
	  | ((Not (_,t1,_),Not (_,t2,_)) | (Star (_,t1,_),Star (_,t2,_))) -> 
	    oldcomp t1 t2
	  | _ -> Pervasives.compare e1 e2)
       in oldcomp )
	

    let compare a b = 
      match extract_uid a, extract_uid b with 
	| (-1,_ | _,-1) -> 
	  old_compare a b
	| uida,uidb -> 
	  let myres = Pervasives.compare uida uidb in 
	  if Decide_Util.debug_mode 
	  then 
	    match myres, old_compare a b
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
      | Assg (_,l,r,o) -> Assg(-1,l,r,o)
      | Test (_,l,r,o) -> Test(-1,l,r,o)
      | Plus (_,es,o) -> Plus(-1,BatSet.PSet.map invalidate_id es,o)
      | Times (_,es,o) -> Times(-1,List.map invalidate_id es,o)
      | Not (_,e,o) -> Not(-1,invalidate_id e,o)
      | Star (_,e,o) -> Star(-1,invalidate_id e,o)
      | other -> other

	
    let hash a = match extract_uid a with 
      | -1 -> Hashtbl.hash (invalidate_id a)
      | a -> Hashtbl.hash a


  end

 
open Term
type 'a term = 'a Term.t

module UnivMap = Decide_Util.SetMapF (Field) (Value)
 
type 'a formula = Eq of 'a Term.t * 'a Term.t
	       | Le of 'a Term.t * 'a Term.t


let zero a = Zero (0,a)
let one a = One (1,a)
let dup a = Dup (2,a)


(***********************************************
 * output
 ***********************************************)


let term_to_string = Term.to_string

let formula_to_string (e : 'a formula) : string =
  match e with
  | Eq (s,t) -> Term.to_string ( s) ^ " == " ^ 
    Term.to_string ( t)
  | Le (s,t) -> Term.to_string ( s) ^ " <= " ^ 
    Term.to_string ( t)

let termset_to_string (ts : ('a term) BatSet.PSet.t) : string =
  let l = BatSet.PSet.elements ts in
  let m = List.map term_to_string l in
  String.concat "\n" m

  
(***********************************************
 * utilities
 ***********************************************)

let terms_in_formula (f : 'a formula) =
  match f with (Eq (s,t) | Le (s,t)) -> s,t

let rec is_test (t : 'a term) : bool =
  match t with
  | Assg _ -> false
  | Test _ -> true
  | Dup _ -> false
  | Times (_,x,_) -> List.for_all is_test x
  | Plus (_,x,_) -> BatSet.PSet.for_all is_test x
  | Not (_,x,_) -> is_test x || failwith "May not negate an action"
  | Star (_,x,_) -> is_test x
  | (Zero _ | One _) -> true

let rec vars_in_term (t : 'a term) : Field.t list =
  match t with
  | (Assg (_,x,_,_) | Test(_,x,_,_)) -> [x]
  | Times (_,x,_) -> List.concat (List.map vars_in_term x)
  | Plus (_,x,_) -> List.concat (List.map vars_in_term (BatSet.PSet.elements x))
  | (Not (_,x,_) | Star (_,x,_)) -> vars_in_term x
  | (Dup _ | Zero _ | One _) -> []


(* Collect the possible values of each variable *)
let values_in_term (t : 'a term) : UnivMap.t =
  let rec collect (t : 'a term) (m : UnivMap.t) : UnivMap.t =
  match t with 
  | (Assg (_,x,v,_) | Test (_,x,v,_)) -> UnivMap.add x v m
  | Plus (_,s,_) -> BatSet.PSet.fold collect s m
  | Times (_,s,_) -> List.fold_right collect s m
  | (Not (_,x,_) | Star (_,x,_)) -> collect x m
  | (Dup _ | Zero _ | One _) -> m in
  collect t UnivMap.empty

let rec contains_a_neg term = 
  match term with 
    | (Assg _ | Test _ | Dup _ | Zero _ | One _) -> false
    | Not _ -> true
    | Times (_,x,_) -> List.fold_left 
      (fun acc x -> acc || (contains_a_neg x)) false x
    | Plus (_,x,_) -> BatSet.PSet.fold 
      (fun x acc -> acc || (contains_a_neg x)) x false 
    | Star (_,x,_) -> contains_a_neg x



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
let flatten_sum o (t : 'a Term.t list) : 'a Term.t =
  let open Term in 
  let f (x : 'a Term.t) = 
    match x with 
      | Plus (_,v,_) -> 
	(BatSet.PSet.elements v) 
      | (Zero _) -> [] 
      | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  let t2 = BatSet.PSet.of_list t1 in
  match BatSet.PSet.elements t2 with 
    | [] -> zero o
    | [x] -> x
    | _ ->  (Term.Plus (-1, t2, o))
    
let flatten_product o (t : 'a Term.t list) : 'a Term.t =
  let f x = match x with 
    | Times (_,v,_) -> v 
    | One _  -> [] 
    | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  if List.exists 
    (fun x -> match x with (Zero _)  -> true | _ -> false) t1 
  then zero o
  else match t1 with 
    | [] -> one o
    | [x] -> x 
    | _ ->  (Term.Times (-1,t1,o))
    
let flatten_not o (t : 'a Term.t) : 'a Term.t =
  match t with
  | Not (_,y,_) -> y
  | Zero _ -> one o
  | One _ ->  zero o
  | _ -> (Term.Not (-1,t,o))

let flatten_star o (t : 'a Term.t) : 'a Term.t =
  let t1 = match t with
  | Plus (_,x,o) -> 
    flatten_sum o (List.filter (fun s -> not (is_test s))
		   (BatSet.PSet.elements x))
  | _ -> t in
  if is_test t1 then one o
  else match t1 with
  | Star _ -> t1
  | _ -> (Term.Star (-1,t1,o))
    
let rec simplify (t : 'a Term.t) : 'a Term.t =
  match t with
  | Plus (_,x,o) -> flatten_sum o (List.map simplify 
			       (BatSet.PSet.elements x))
  | Times (_,x,o) -> flatten_product o (List.map simplify x)
  | Not (_,x,o) -> flatten_not o (simplify x)
  | Star (_,x,o) -> flatten_star o (simplify x)
  | _ -> t

    
let contains_dups (t : 'a term) : bool =
  let rec contains t =
    match t with 
    | (Assg _ | Test _ | Zero _ | One _) -> false
    | Dup _ -> true
    | Plus (_,x,_) ->  
      (BatSet.PSet.fold (fun e acc ->  (contains e) || acc)  x false)
    | Times (_,x,_) ->  
      (List.fold_left (fun acc e -> (contains e) || acc) false x)
    | Not (_,x,_) ->  (contains x)
    | Star (_,x,_) ->  (contains x) in
  contains t


(* apply De Morgan laws to push negations down to the leaves *)
let deMorgan df (t : 'a term) : 'a term =
  let rec dM (t : 'a term) : 'a term =
    let f o x = dM (Not (-1,x,o)) in
    match t with 
      | (Assg _ | Test _ | Zero _ | One _ | Dup _) -> t
      | Star (_, x, o) -> Star (-1, dM x, o)
      | Plus (_,x, o) -> Plus (-1,BatSet.PSet.map dM x, o)
      | Times (_,x, o) -> Times (-1,List.map dM x, o)
      | Not (_,(Not (_,x,_)),_) -> dM x
      | Not (_,(Plus (_,s,_)),_) -> Times (-1, List.map (fun e -> (f (df ()) e)) (BatSet.PSet.elements s), df())
      | Not (_,Times (_,s,_),_) -> Plus (-1, BatSet.PSet.of_list (List.map (fun e -> f (df ()) e) s), df())
      | Not (_,Star (_,x,_),_) ->
	if is_test x then zero (df())
	else failwith "May not negate an action"
      | Not (_, (Zero _),_ ) -> one (df())
      | Not (_, (One _),_ ) -> zero (df())
      | Not(_, (Dup _),_) -> failwith "you may not negate a dup!"
      | Not(_, (Assg _),_) -> failwith "you may not negate an assg!"
      | Not (_, (Test _),_) -> t in
  simplify (dM t)


(* smart constructors *)

let assign_ids : 'a Term.t -> 'a Term.t = 
  let timeshash = Hashtbl.create 100 in 
  let plushash = Hashtbl.create 100 in 
  let nothash = Hashtbl.create 100 in 
  let starhash = Hashtbl.create 100 in 
  let testhash = Hashtbl.create 100 in 
  let assghash = Hashtbl.create 100 in 
  let getinassg : Field.t * Value.t -> 'a Term.t = Hashtbl.find assghash in 
  let setinassg : Field.t * Value.t -> 'a Term.t -> unit = Hashtbl.replace assghash in 
  let extract_uid t = 
    match extract_uid t with 
      | -1 -> failwith "BAD! ASSIGNING BASED ON -1 UID"
      | i -> i in 
  let ids_from_list tl = 
    List.map extract_uid tl in
  let ids_from_set ts = 
    BatSet.PSet.fold (fun t acc -> (extract_uid t) :: acc) ts [] in
  let counter = ref 3 in 
  let getandinc _ = 
    let ret = !counter in 
    if ret > 1073741822
    then failwith "you're gonna overflow the integers, yo";
    counter := !counter + 1; 
    ret in 
  let rec assign_ids (t : 'a Term.t) : 'a Term.t = 
    match t with 
      | Dup (-1,o) -> dup o
      | One (-1,o) -> one o
      | Zero (-1,o) -> zero o
      | Assg(-1,f,v,o) -> 
	(try getinassg (f,v)
	with Not_found -> 
	  let ret = Assg(getandinc (), f,v,o) in 
	  Hashtbl.replace assghash (f,v) ret; 
	  ret)
      | Test(-1,f,v,o) -> 
	(try Hashtbl.find testhash (f,v)
	 with Not_found -> 
	   let ret = Test(getandinc (), f,v,o) in 
	   Hashtbl.replace testhash (f,v) ret; 
	   ret)
      | Plus (-1,ts,o) -> 
	let ts = BatSet.PSet.map assign_ids ts in 
	let ids = (ids_from_set ts) in 
	(try Hashtbl.find plushash ids 
	 with Not_found -> 
	   let ret = Plus(getandinc (), ts,o) in 
	   Hashtbl.replace plushash ids ret;
	   ret
	)
      | Times (-1,tl,o) -> 
	let tl = List.map assign_ids tl in
	let ids = ids_from_list tl in 
	(try Hashtbl.find timeshash ids 
	 with Not_found -> 
	   let ret = Times(getandinc (), tl,o) in 
	   Hashtbl.replace timeshash ids ret;
	   ret
	)
      | Not (-1, t,o) -> 
	let t = assign_ids t in 
	(try Hashtbl.find nothash (extract_uid t)
	 with Not_found -> 
	  let ret = Not(getandinc (), t,o) in 
	  Hashtbl.replace nothash (extract_uid t) ret;
	  ret)
      | Star (-1, t,o) -> 
	let t = assign_ids t in 
	(try Hashtbl.find starhash (extract_uid t)
	with Not_found -> 
	  let ret = Star(getandinc (), t,o) in 
	  Hashtbl.replace starhash (extract_uid t) ret;
	  ret)
      | already_assigned -> already_assigned
	  
  in
  assign_ids

  let make_assg (f,v) o = 
    assign_ids (Assg(-1,f,v,o)) 

  let make_test (f,v,o) = 
    assign_ids (Test(-1,f,v,o)) 
      
  let make_dup o = dup o

  let make_plus ts o = 
    assign_ids (flatten_sum o ts)

  let make_times tl o = 
    assign_ids (flatten_product o tl)
    
  let make_star t o = 
    assign_ids (flatten_star o t)
      
  let make_zero o = zero o
    
  let make_one o  = one o

  let parse_and_simplify df (parse : string -> 'a formula) (s : string) : 'a formula = 
    match parse s with 
      | Eq (l,r) -> Eq(assign_ids (deMorgan df (simplify l)), assign_ids (deMorgan df (simplify r)))
      | Le (l,r) -> Le(assign_ids (deMorgan df (simplify l)), assign_ids (deMorgan df (simplify r)))



let hits = ref 0 
let misses = ref 1 

let memoize (f : 'b Term.t -> 'a) =
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
    let hash = ref BatMap.PMap.empty in 
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



(* set dups to 0 *)
let zero_dups (t : 'a term) : 'a term =
  let zval = zero in
  let rec zero (t : 'a term) =
    match t with 
      | (Assg _ | Test _ | Zero _ | One _ ) -> t
      | Dup (_,o) -> zval o
      | Plus (_,x,o) -> Plus (-1, BatSet.PSet.map zero x,o)
      | Times (_,x,o) -> Times (-1, List.map zero x,o)
      | Not (_,x,o) -> Not (-1, zero x,o)
      | Star (_,x,o) -> Star (-1, zero x,o) in
  let zero = memoize zero in 
  assign_ids (simplify (zero t))

(* set dups to 1 *)
let one_dups (t : 'a term) : 'a term =
  let oval = one in 
  let rec one t =
    match t with 
      | (Assg _ | Test _ | Zero _ | One _) -> t
      | Dup (_,o) -> oval o
      | Plus (_,x,o) -> Plus (-1, BatSet.PSet.map one x, o)
      | Times (_,x,o) -> Times (-1, List.map one x, o)
      | Not (_,x,o) -> Not (-1, one x, o)
      | Star (_,x,o) -> Star (-1, one x, o) in
  let one = memoize one in
  assign_ids (simplify (one t))
    
