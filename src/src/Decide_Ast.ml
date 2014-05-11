open Decide_Util

exception Empty

let utf8 = ref false 

(***********************************************
 * syntax
 ***********************************************)

     
  module rec Term : sig
    module Field : sig
      type t
      val compare : t -> t -> int
      val to_string : t -> string
      val of_string : string -> t
    end
    module Value : sig
      type t 
      val compare : t -> t -> int
      val to_string : t -> string
      val of_string : string -> t
      val extra_val : t
    end
      
    type uid
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
  val to_string : t -> string
  val to_string_sexpr : t -> string
  val old_compare : (t -> t -> int) ref
  val compare : t -> t -> int
  val uid_of_int : int -> uid
  val int_of_uid : uid -> int
  val ts_elements : (TermSet.t -> t list) ref
  end = struct 
    type uid = int	
	
    module Field = struct 
      type t = int
      let compare = Pervasives.compare
      let of_string,to_string = 
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
	of_string,to_string
    end

    module Value = struct 
      type t = int
      let compare = Pervasives.compare
      let of_string,to_string = 
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
	of_string,to_string
      let extra_val = -1
    end

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

    let old_compare : (t -> t -> int) ref = ref (fun _ _ -> failwith "dummy")

    let compare a b = 
      let myres = Pervasives.compare (extract_uid a) (extract_uid b) in 
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



module rec InitialTerm : sig
  type t =
  | Assg of Term.Field.t * Term.Value.t
  | Test of Term.Field.t * Term.Value.t
  | Dup
  | Plus of InitialTermSet.t
  | Times of t list
  | Not of t
  | Star of t
  | Zero
  | One
  val to_term : t -> Term.t
  val of_term : Term.t -> t
  val compare : t -> t -> int
  val to_string_ocaml : t -> string

end = struct 
  type t =
    | Assg of Term.Field.t * Term.Value.t
    | Test of Term.Field.t * Term.Value.t
    | Dup
    | Plus of InitialTermSet.t
    | Times of t list
    | Not of t
    | Star of t
    | Zero
    | One
	
  let rec compare a b = 
    match a,b with 
    | Plus ts1, Plus ts2 -> 
      let cardinal1 = InitialTermSet.cardinal ts1 in
      let cardinal2 = InitialTermSet.cardinal ts2 in
      if cardinal2 = cardinal1
      then 
	List.fold_right2
	  (fun l r acc -> 
	    if acc = 0 
	    then compare l r
	    else acc) 
	  (List.fast_sort compare (InitialTermSet.elements ts1)) 
	  (List.fast_sort compare (InitialTermSet.elements ts2)) 0 
      else if cardinal1 < cardinal2 
      then -1
      else 1
    | Times tl1, Times tl2 -> 
      let len1 = List.length tl1 in 
      let len2 = List.length tl2 in
      if len1 = len2
      then List.fold_right2 
	(fun l r acc -> 
	  if acc = 0 
	  then compare l r
	  else acc) (tl1) ( tl2) 0
      else if len1 < len2
      then -1
      else 1
    | Star a,Star b -> 
      compare a b
    | Not a, Not b -> 
      compare a b
    | _ -> Pervasives.compare a b



  let of_term e = 
    let module TTerm = InitialTerm in 
    let rec rf e = 
      match e with 
	| Term.Assg (_,k,v) -> TTerm.Assg (k,v) 
	| Term.Test (_,k,v)-> TTerm.Test (k,v)
	| Term.Dup _ -> TTerm.Dup
	| Term.Plus (_,ts)-> TTerm.Plus 
	  (TermSet.fold (fun x -> InitialTermSet.add (rf x)) 
	     ts InitialTermSet.empty)
	| Term.Times (_,tl)-> TTerm.Times (List.map rf tl)
	| Term.Not (_,tm)-> TTerm.Not (rf tm)
	| Term.Star (_,tm)-> TTerm.Star (rf tm)
	| Term.Zero _ -> TTerm.Zero
	| Term.One _ -> TTerm.One
    in rf e

  let _ = Term.old_compare := (fun a b -> compare (of_term a) (of_term b))

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
      | Assg ( var, value) -> Printf.sprintf "%s:=%s" 
	(Term.Field.to_string var) (Term.Value.to_string value)
      | Test ( var, value) -> Printf.sprintf "%s=%s" 
	(Term.Field.to_string var) (Term.Value.to_string value)
      | Dup  -> "dup"
      | Plus (x) -> assoc_to_string " + " "0" 
	(List.map protect ( InitialTermSet.elements x ))
      | Times (x) -> assoc_to_string ";" "1" (List.map protect x)
      | Not (x) -> (if !utf8 then "¬" else "~") ^ (protect x)
      | Star (x) -> (protect x) ^ "*"
      | Zero  -> "drop"
      | One  -> "pass"

  let rec to_string_sexpr = function 
    | Assg ( var, value) -> Printf.sprintf "(%s:=%s)" 
      (Term.Field.to_string var) (Term.Value.to_string value)
    | Test ( var, value) -> Printf.sprintf "(%s=%s)" 
      (Term.Field.to_string var) (Term.Value.to_string value)
    | Dup  -> "dup"
    | Plus (x) -> 
      Printf.sprintf "(+ %s)" 
	(InitialTermSet.fold 
	   (fun x -> Printf.sprintf "%s %s" (to_string_sexpr x)) x "")
    | Times (x) -> 
      Printf.sprintf "(; %s)" 
	(List.fold_right 
	   (Printf.sprintf "%s %s") (List.map to_string_sexpr x) "")
    | Not (x) -> (if !utf8 then "¬" else "~") ^ 
      (Printf.sprintf "(%s)" (to_string_sexpr x))
    | Star (x) -> (Printf.sprintf "(%s)" (to_string_sexpr x)) ^ "*"
    | Zero  -> "drop"
    | One  -> "pass"


  let rec to_string_ocaml = function 
    | Assg ( var, value) -> Printf.sprintf "(%s:=%s)" 
      (Term.Field.to_string var) (Term.Value.to_string value)
    | Test ( var, value) -> Printf.sprintf "(%s=%s)" 
      (Term.Field.to_string var) (Term.Value.to_string value)
    | Dup  -> "dup"
    | Plus (x) -> 
      Printf.sprintf "(+ %s)" 
	(InitialTermSet.fold 
	   (fun x -> Printf.sprintf "%s %s" (to_string_sexpr x)) x "")
    | Times (x) -> 
      Printf.sprintf "(; %s)" (List.fold_right (Printf.sprintf "%s %s") 
				 (List.map to_string_sexpr x) "")
    | Not (x) -> (if !utf8 then "¬" else "~") ^ (Printf.sprintf "(%s)" 
						   (to_string_sexpr x))
    | Star (x) -> (Printf.sprintf "(%s)" (to_string_sexpr x)) ^ "*"
    | Zero  -> "drop"
    | One  -> "pass"


  let to_term = 
    let get_uid,set_term = 
      let counter = ref 0 in 
      let module Map = Map.Make(InitialTerm) in 
      let hash = ref Map.empty in 
      let get_uid e = 
	try let (id : Term.uid),trm = Map.find e !hash in 
	    (if debug_mode
	     then 
		((match trm with 
		  | Some e' -> 
		    (match Map.find (of_term e') !hash with 
		      | id',Some e'' when (id' = id && 
			  (Term.to_string_sexpr e') = 
			  (Term.to_string_sexpr e'')) -> ()
		      | id', Some e'' -> 
			Printf.printf "id: %u.  id': %u. e':  %s.  e'': %s."
			  (Term.int_of_uid id) (Term.int_of_uid id') 
			  (Term.to_string_sexpr e') 
			  (Term.to_string_sexpr e'')
			;
			failwith "hash collision?!?!"
		      | _ -> 
			failwith "get_uid new sanity check fails hard!");
		  | None -> ());id,trm)
	     else 
		id,trm)
	with Not_found -> 
	  let (this_counter : Term.uid) = Term.uid_of_int (!counter) in
	  if !counter > 107374182
	  then failwith "about to overflow the integers!";
	  counter := !counter + 1;
	  hash := Map.add e (this_counter,None) !hash;
	  this_counter,None
      in 
      let set_term e new_e= 
	let id,_ = get_uid e in 
	hash := Map.add e (id,Some new_e) !hash in 
      get_uid,set_term in
    
    let to_term (e : InitialTerm.t) : Term.t = 
      let rec rf e = 
	let module TTerm = InitialTerm in 
	let id,e' = 
	  if debug_mode 
	  then let r,_ = get_uid e in 
	       r,None
	  else get_uid e
	in 
	match e' with 
	  | Some e -> e
	  | None -> (
	    match e with 
	      | TTerm.Assg (k,v) -> let e' = Term.Assg(id, k, v) in 
				    set_term e e'; e'
	      | TTerm.Test (k,v) -> let e' = Term.Test(id, k, v) in 
				    set_term e e'; e'
	      | TTerm.Dup -> let e' = Term.Dup(id) in 
			     set_term e e'; e'
	      | TTerm.Plus (ts) -> 
		let e' = 
		  Term.Plus(id, 
			    InitialTermSet.fold 
			      (fun x -> TermSet.add (rf x) ) 
			      ts TermSet.empty) in 
		set_term e e'; e'
	      | TTerm.Times (tl) -> 
		let e' = Term.Times(id, List.map rf tl) in 
		set_term e e'; e'
	      | TTerm.Not tm -> 
		let e' = Term.Not (id, rf tm) in 
		set_term e e'; e'
	      | TTerm.Star tm -> 
		let e' = Term.Star (id, rf tm) in 
		set_term e e'; e'
	      | TTerm.Zero -> 
		let e' = Term.Zero id in 
		set_term e e'; e'
	      | TTerm.One -> 
		let e' = Term.One id in 
		set_term e e'; e')
      in 
      rf e in
    to_term
    
end


and InitialTermSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
  val from_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val return : elt -> t
end with type elt = InitialTerm.t = struct
  include Set.Make (InitialTerm)
  let map (f : elt -> elt) (ts : t) : t =
    fold (fun x -> add (f x)) ts empty
  let from_list (tl : elt list) : t =
    List.fold_right add tl empty
  let bind (ts : t) (f : elt -> t) : t =
    fold (fun x t -> union (f x) t) ts empty
  let return = singleton

end

  

open Term
type term = Term.t

let make_term = InitialTerm.to_term

module UnivMap = Decide_Util.SetMapF (Field) (Value)
 
type formula = Eq of InitialTerm.t * InitialTerm.t
	       | Le of InitialTerm.t * InitialTerm.t

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
  Printf.fprintf file "%s" 
    (match formula with 
      | Eq (s,t) -> 
	Printf.sprintf "Decide_Ast.Eq (%s,%s)" 
	  (InitialTerm.to_string_ocaml s)
	  (InitialTerm.to_string_ocaml t)
      | Le (s,t) -> 
	Printf.sprintf "Decide_Ast.Le (%s,%s)" 
	  (InitialTerm.to_string_ocaml s)
	  (InitialTerm.to_string_ocaml t)
    )

  
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

let rec vars_in_term (t : term) : Term.Field.t list =
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

let zero = (make_term InitialTerm.Zero) 
let one = (make_term InitialTerm.One)

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


let hits = ref 0 
let misses = ref 1 
module TermMap = Map.Make(Term)

let memoize (f : Term.t -> 'a) =
  let hash_version = 
    let hash = ref TermMap.empty in 
    (fun b -> 
      try let ret = TermMap.find b !hash in
	  (hits := !hits + 1;
	   ret)
      with Not_found -> 
	(misses := !misses + 1;
	 let ret = f b in 
	 hash := TermMap.add b ret !hash;
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
			 (Term.Field.of_string "x", 
			  Term.Value.of_string "3")) 
		      (InitialTermSet.singleton 
			 (InitialTerm.Not 
			    (InitialTerm.Test
			       (Term.Field.of_string "x", 
				Term.Value.of_string "3")))))) in
    assert (0 = (InitialTerm.compare value value))
  else ()
    
