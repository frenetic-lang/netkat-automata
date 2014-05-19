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

  (* utilities for Map, Hashtbl, etc *)
  val old_compare : (t -> t -> int) ref
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool


  (* because module troubles *)
  val largest_uid : unit -> uid
  val ts_elements : (TermSet.t -> t list) ref
  val extract_uid : t -> uid

  end = struct 
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


    let uid_of_int x = x
    let int_of_uid x = x
    let largest_uid _ = !biggest_int

    let old_compare : (t -> t -> int) ref = ref (fun _ _ -> failwith "dummy")

    (* TODO: this compare function is going to be "broken" with respect to 0/1 dups, 
       in the sense that the pre- and post- zero/one-dup term will be considered equal.
       I think this is fine, but it certainly bares thinking about.
    *)
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

    let hash a = int_of_uid (extract_uid a)


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
  (* Fingers crossed...*)
  let _ = Term.ts_elements := Obj.magic elements
end  

let _ = Term.old_compare := 
  (let rec oldcomp = (fun a b -> 
    match a,b with 
      | (Plus (_,al)),(Plus (_,bl)) -> TermSet.compare al bl
      | (Times (_,al)),(Times (_,bl)) -> 
	(match List.length al, List.length bl with 
	  | a,b when a = b -> 
	    List.fold_right2 (fun a b acc -> 
	      if (acc = 0) 
	      then oldcomp a b 
	      else acc
	    ) al bl 0
	  | a,b when a < b -> -1 
	  | a,b when a > b -> 1
	)
      | (Not (_,t1),Not (_,t2)) -> 
      | (Star (_,t1),Star (_,t2)) -> 
	oldcomp t1 t2
      | _ -> Pervasives.compare a b)
   in oldcomp )
    
open Term
type term = Term.t

module UnivMap = Decide_Util.SetMapF (Field) (Value)
 
type formula = Eq of Term.t * Term.t
	       | Le of Term.t * Term.t

(***********************************************
 * output
 ***********************************************)


let term_to_string = Term.to_string

let formula_to_string (e : formula) : string =
  match e with
  | Eq (s,t) -> Term.to_string (make_term s) ^ " == " ^ 
    Term.to_string (make_term t)
  | Le (s,t) -> Term.to_string (make_term s) ^ " <= " ^ 
    Term.to_string (make_term t)

let termset_to_string (ts : TermSet.t) : string =
  let l = TermSet.elements ts in
  let m = List.map term_to_string l in
  String.concat "\n" m

let serialize_formula formula file = 
  let file = open_out file in 
  Printf.fprintf file "let serialized_formula = %s" 
    (match formula with 
      | Eq (s,t) -> 
	Printf.sprintf "Decide_Ast.Eq (%s,%s)" 
	  (InitialTerm.to_string_ocaml s)
	  (InitialTerm.to_string_ocaml t)
      | Le (s,t) -> 
	Printf.sprintf "Decide_Ast.Le (%s,%s)" 
	  (InitialTerm.to_string_ocaml s)
	  (InitialTerm.to_string_ocaml t)
    );
  close_out file

  
(***********************************************
 * utilities
 ***********************************************)

let terms_in_formula (f : formula) =
  match f with (Eq (s,t) | Le (s,t)) -> (make_term s,make_term t)

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
  let rec collect (t : term) (h : UnivMap.t) : UnivMap.t =
  match t with 
  | (Assg (_,x,v) | Test (_,x,v)) -> UnivMap.add x v h
  | Plus (_,s) -> TermSet.fold collect s h
  | Times (_,s) -> List.fold_right collect s h
  | (Not (_,x) | Star (_,x)) -> collect x h
  | (Dup _ | Zero _ | One _) -> h in
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

(* flatten terms *)
let flatten_sum (t : InitialTerm.t list) : InitialTerm.t =
  let open InitialTerm in 
  let f (x : InitialTerm.t) = match x with Plus (v) -> 
    (InitialTermSet.elements v) | (Zero ) -> [] | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  let t2 = InitialTermSet.from_list t1 in
  match InitialTermSet.elements t2 with [] -> 
    InitialTerm.Zero | [x] -> ( x) | _ ->  (InitialTerm.Plus t2)
    
let flatten_product (t : InitialTerm.t list) : InitialTerm.t =
  let open InitialTerm in 
  let f x = match x with Times (v) -> v | One  -> [] | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  if List.exists (fun x -> match x with Zero  -> true | _ -> false) 
    t1 then ( InitialTerm.Zero)
  else match t1 with [] -> ( InitialTerm.One) | [x] -> x | _ ->  
    (InitialTerm.Times  t1)
    
let flatten_not (t : InitialTerm.t) : InitialTerm.t =
  let open InitialTerm in 
  match t with
  | Not (y) -> y
  | Zero  -> InitialTerm.One
  | One  ->  InitialTerm.Zero
  | _ -> (InitialTerm.Not t)

let is_test_tt t = 
  let open InitialTerm in
  let rec is_test (t : InitialTerm.t) : bool =
    match t with
      | Assg _ -> false
      | Test _ -> true
      | Dup  -> false
      | Times (x) -> List.for_all is_test x
      | Plus (x) -> InitialTermSet.for_all is_test x
      | Not (x) -> is_test x || failwith "May not negate an action"
      | Star (x) -> is_test x
      | (Zero | One ) -> true in 
  is_test t

    
let flatten_star (t : InitialTerm.t) : InitialTerm.t =
  let open InitialTerm in
  
  let t1 = match t with
  | Plus (x) -> flatten_sum (List.filter (fun s -> not (is_test_tt s)) 
			       (InitialTermSet.elements x))
  | _ -> t in
  if is_test_tt t1 then InitialTerm.One
  else match t1 with
  | Star _ -> t1
  | _ -> (InitialTerm.Star t1)
    
let rec simplify_tt (t : InitialTerm.t) : InitialTerm.t =
  let open InitialTerm in 
  match t with
  | Plus (x) -> flatten_sum (List.map simplify_tt 
			       (InitialTermSet.elements x))
  | Times (x) -> flatten_product (List.map simplify_tt x)
  | Not (x) -> flatten_not (simplify_tt x)
  | Star (x) -> flatten_star (simplify_tt x)
  | _ -> t

let simplify t = 
  (make_term (simplify_tt (InitialTerm.of_term t)))

let simplify_formula (e : formula) : formula =
  match e with
  | Eq (s,t) -> Eq ((simplify_tt s),  (simplify_tt t))
  | Le (s,t) -> Le ( (simplify_tt s),  (simplify_tt t))

(* set dups to 0 *)
let zero_dups (t : term) : term =
  let rec zero (t : InitialTerm.t) =
    let open InitialTerm in 
	match t with 
	  | (Assg _ | Test _ | Zero | One ) -> t
	  | Dup  -> InitialTerm.Zero
	  | Plus (x) -> Plus (InitialTermSet.map zero x)
	  | Times x -> Times (List.map zero x)
	  | Not x -> Not (zero x)
	  | Star x -> Star (zero x) in
  (make_term (simplify_tt (zero (InitialTerm.of_term t))))

(* set dups to 1 *)
let one_dups (t : term) : term =
  let open InitialTerm in 
  let rec one t =
    match t with 
      | (Assg _ | Test _ | Zero | One) -> t
      | Dup -> One
      | Plus x -> Plus (InitialTermSet.map one x)
      | Times x -> Times (List.map one x)
      | Not x -> Not (one x)
      | Star x -> Star (one x) in
  (make_term (simplify_tt (one (InitialTerm.of_term t))))

let zero = Zero 0
let one = One 1
let dup = Dup 2 in 

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
  let open InitialTerm in
  let rec dM (t : InitialTerm.t) : InitialTerm.t =
    let f x = dM (Not x) in
    match t with 
      | (Assg _ | Test _ | Zero | One | Dup) -> t
      | Plus x -> Plus (InitialTermSet.map dM x)
      | Times x -> Times (List.map dM x)
      | Not (Not x) -> dM x
      | Not (Plus s) -> Times (List.map f (InitialTermSet.elements s))
      | Not (Times s) -> Plus (InitialTermSet.from_list (List.map f s))
      | Not (Star x) ->
	if is_test_tt x then Zero
      else failwith "May not negate an action"
      | Not Zero -> One
      | Not One -> Zero
      | Not _ -> t
      | Star x -> Star (dM x) in
  (make_term (simplify_tt (dM (InitialTerm.of_term t))))



(* smart constructors *)

let assign_ids = 
  let hash = Hashtbl.create 100 in 
  let testhash = Hashtbl.create 100 in 
  let assghash = Hashtbl.create 100 in 
  let extract_uid t = 
    match extract_uid t with 
      | -1 -> failwith "BAD! ASSIGNING BASED ON -1 UID"
      | i -> i in 
  let ids_from_list tl = 
    List.map extract_uid tl in
  let ids_from_set ts = 
    TermSet.fold (fun f acc -> (extract_uid f) :: acc) ts []
  let counter = ref 3 in 
  let getandinc  = 
    let ret = !counter in 
    counter := !counter + 1; 
    ret in 
  let rec assign_ids t = 
    match t with 
      | Dup -> dup
      | One -> one 
      | Zero -> zero 
      | Assg(-1,f,v) -> 
	(try Hashtbl.find assghash (f,v)
	with Not_found -> 
	  let ret = Assg(getandinc (), f,v) in 
	  Hashtbl.replace assghash (f,v) ret; 
	  ret)
      | Test(-1,f,v) -> 
	(try Hashtbl.find assghash (f,v)
	 with Not_found -> 
	   let ret = Assg(getandinc (), f,v) in 
	   Hashtbl.replace assghash (f,v) ret; 
	   ret)
      | Plus (-1,ts) -> 
	let ids = (ids_from_set ts) in 
	(try Hashtbl.find hash ids 
	 with Not_found -> 
	   let ret = Plus(getandinc (), ts) in 
	   Hashtbl.replace hash ids ret;
	   ret
	)
      | Times (-1,tl) -> 
	let ids = ids_from_list tl in 
	(try Hashtbl.find hash ids 
	 with Not_found -> 
	   let ret = Times(getandinc (), tl) in 
	   Hashtbl.replace hash ids ret;
	   ret
	)
      | Not (-1, t) -> 
	(try Hashtbl.find hash [t] 
	with Not_found -> 
	  let ret = Not(getandinc (), t) in 
	  Hashtbl.replace hash [t] ret;
	  ret)
      | Star (-1, t) -> 
	try Hashtbl.find hash [t]
	with Not_found -> 
	  let ret = Star(getandinc (), t) in 
	  Hashtbl.replace hash [t] ret;
	  ret
  in
  assign_ids

  let make_assg f v = 
    assign_ids (Assg(-1f,v)) 

  let make_test f v = 
    assign_ids (test(-1,f,v)) 
      
  let make_dup = dup i

  let make_plus ts = 
    assign_ids (flatten_sum (Plus(-1,ts))) 

  let make_times tl = 
    assign_ids (flatten_product (Times(-1,tl))) 

  let make_not t = 
    assign_ids (deMorgan (flatten_not (Not(-1,t)))) 
      
  let make_star t = 
    assign_ids (flatten_star (Star(-1,t))) 
      
  let make_zero = zero 
    
  let make_one  = one 

  let parse_and_simplify parse s = 
    assign_ids (deMorgan (simplify (parse s)))


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


let _ = 
  if debug_mode
  then 
    let value = (InitialTerm.Plus
		   (InitialTermSet.add 
		      (InitialTerm.Test
			 (Field.of_string "x", 
			  Value.of_string "3")) 
		      (InitialTermSet.singleton 
			 (InitialTerm.Not 
			    (InitialTerm.Test
			       (Field.of_string "x", 
				Value.of_string "3")))))) in
    assert (0 = (InitialTerm.compare value value))
  else ()
    
