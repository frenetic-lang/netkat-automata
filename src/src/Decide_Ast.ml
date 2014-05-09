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
  val compare : t -> t -> int
  val uid_of_int : int -> uid
  val ts_elements : (TermSet.t -> t list) ref
  end = struct 
    type uid = int	

    module Field = struct 
      type t = string
      let compare = Pervasives.compare
      let to_string x = x
      let of_string x = x
    end
    module Value = struct 
      type t = string
      let compare = Pervasives.compare
      let to_string x = x
      let of_string x = x
      let extra_val = "☃"
    end

    let compare = Pervasives.compare

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

    let int_to_uid (x : int) : uid = x

    let ts_elements : (TermSet.t -> t list) ref  = ref (fun _ -> failwith "module issues")

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
      let assoc_to_string (op : string) (ident : string) (s : string list) : string =
	match s with
	  | [] -> ident
	  | _ -> String.concat op s in
      match t with
	| Assg (_, var, value) -> Printf.sprintf "%s:=%s" (Field.to_string var) (Value.to_string value)
	| Test (_, var, value) -> Printf.sprintf "%s=%s" (Field.to_string var) (Value.to_string value)
	| Dup _ -> "dup"
	| Plus (_,x) -> assoc_to_string " + " "0" (List.map protect ( !ts_elements x ))
	| Times (_,x) -> assoc_to_string ";" "1" (List.map protect x)
	| Not (_,x) -> (if !utf8 then "¬" else "~") ^ (protect x)
	| Star (_,x) -> (protect x) ^ "*"
	| Zero _ -> "drop"
	| One _ -> "pass"
    let uid_of_int x = x

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
	
  let compare = Pervasives.compare

  let of_term e = 
    let module TTerm = InitialTerm in 
    let rec rf e = 
      match e with 
	| Term.Assg (_,k,v) -> TTerm.Assg (k,v) 
	| Term.Test (_,k,v)-> TTerm.Test (k,v)
	| Term.Dup _ -> TTerm.Dup
	| Term.Plus (_,ts)-> TTerm.Plus 
	  (TermSet.fold (fun x -> InitialTermSet.add (rf x)) ts InitialTermSet.empty)
	| Term.Times (_,tl)-> TTerm.Times (List.map rf tl)
	| Term.Not (_,tm)-> TTerm.Not (rf tm)
	| Term.Star (_,tm)-> TTerm.Star (rf tm)
	| Term.Zero _ -> TTerm.Zero
	| Term.One _ -> TTerm.One
    in rf e

    let get_uid,set_term = 
      let counter = ref 0 in 
      let hash = Hashtbl.create 0 in 
      let get_uid e = 
	try Hashtbl.find hash e 
	with Not_found -> 
	  let (this_counter : Term.uid) = Term.uid_of_int (!counter) in
	  counter := !counter + 1;
	  Hashtbl.replace hash e (this_counter,None);
	  this_counter,None
      in 
      let set_term e new_e= 
	let id,_ = get_uid e in 
	Hashtbl.replace hash e (id,Some new_e) in 
      get_uid,set_term
	
    let to_term (e : InitialTerm.t) : Term.t = 
      let rec rf e = 
	let module TTerm = InitialTerm in 
	let id,e' = get_uid e in 
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
	      | TTerm.Plus (ts) -> let e' = Term.Plus(id, InitialTermSet.fold (fun x -> TermSet.add (rf x) ) ts TermSet.empty) in 
				   set_term e e'; e'
	      | TTerm.Times (tl) -> let e' = Term.Times(id, List.map rf tl) in 
				    set_term e e'; e'
	      | TTerm.Not tm -> let e' = Term.Not (id, rf tm) in 
				set_term e e'; e'
	      | TTerm.Star tm -> let e' = Term.Star (id, rf tm) in 
				 set_term e e'; e'
	      | TTerm.Zero -> let e' = Term.Zero id in 
			      set_term e e'; e'
	      | TTerm.One -> let e' = Term.One id in 
			     set_term e e'; e')
      in 
      rf e

	  
  let make_term (e : InitialTerm.t) : Term.t = 
    match get_uid e with 
      | (_,Some e) -> e
      | _ -> to_term e
    
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
  | Eq (s,t) -> Term.to_string (make_term s) ^ " == " ^ Term.to_string (make_term t)
  | Le (s,t) -> Term.to_string (make_term s) ^ " <= " ^ Term.to_string (make_term t)

let termset_to_string (ts : TermSet.t) : string =
  let l = TermSet.elements ts in
  let m = List.map term_to_string l in
  String.concat "\n" m

(***********************************************
 * coterms
 ***********************************************)

(* coterm representation - a function from positions to subterms *)
(* a position is an integer list where the head gives the *)
(* child number of this term relative to its parent *)
(* The tail is the position of the parent, [] is the root *)

type coterm = (int list, term) Hashtbl.t

let coterm (t : term) : coterm =
  let h = Hashtbl.create 11 in
  let rec siblings n pos s =
    match s with
    | [] -> ()
    | x :: r ->
      subterms (n :: pos) x;
      siblings (n+1) pos r
  and subterms (pos : int list) (t : term) : unit =
    Hashtbl.add h pos t;
    match t with
    | (Assg _ | Test _ | Zero _ | One _ | Dup _) -> ()
    | Plus (_,x) -> siblings 0 pos (TermSet.elements x)
    | Times (_,x) -> siblings 0 pos x
    | Not (_,x) -> subterms (0 :: pos) x
    | Star (_,x) -> subterms (0 :: pos) x in
  subterms [] t; h

let pos_to_string (pos : int list) : string =
  match pos with
  | [] -> "e"
  | _ -> String.concat "" (List.map string_of_int pos)

let coterm_to_string (h : coterm) : string =
  let f pos t r = Printf.sprintf "%s: %s" (pos_to_string pos) (term_to_string t) :: r in
  let s = Hashtbl.fold (fun pos t r -> f pos t r) h [] in
  String.concat "\n" s
  
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
    | Times (_,x) -> List.fold_left (fun acc x -> acc || (contains_a_neg x)) false x
    | Plus (_,x) -> TermSet.fold (fun x acc -> acc || (contains_a_neg x)) x false 
    | Star (_,x) -> contains_a_neg x



(***********************************************
 * simplify
 ***********************************************)

(* flatten terms *)
let flatten_sum (t : InitialTerm.t list) : InitialTerm.t =
  let open InitialTerm in 
  let f (x : InitialTerm.t) = match x with Plus (v) -> (InitialTermSet.elements v) | (Zero ) -> [] | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  let t2 = InitialTermSet.from_list t1 in
  match InitialTermSet.elements t2 with [] -> InitialTerm.Zero | [x] -> ( x) | _ ->  (InitialTerm.Plus t2)
    
let flatten_product (t : InitialTerm.t list) : InitialTerm.t =
  let open InitialTerm in 
  let f x = match x with Times (v) -> v | One  -> [] | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  if List.exists (fun x -> match x with Zero  -> true | _ -> false) t1 then ( InitialTerm.Zero)
  else match t1 with [] -> ( InitialTerm.One) | [x] -> x | _ ->  (InitialTerm.Times  t1)
    
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
  | Plus (x) -> flatten_sum (List.filter (fun s -> not (is_test_tt s)) (InitialTermSet.elements x))
  | _ -> t in
  if is_test_tt t1 then InitialTerm.One
  else match t1 with
  | Star _ -> t1
  | _ -> (InitialTerm.Star t1)
    
let rec simplify_tt (t : InitialTerm.t) : InitialTerm.t =
  let open InitialTerm in 
  match t with
  | Plus (x) -> flatten_sum (List.map simplify_tt (InitialTermSet.elements x))
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
    | Plus (_,x) ->  (TermSet.fold (fun e acc ->  (contains e) || acc)  x false)
    | Times (_,x) ->  (List.fold_left (fun acc e -> (contains e) || acc) false x)
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

let add_terms (t1 : term) (t2 : term) : term =
  make_term (simplify_tt (InitialTerm.Plus (InitialTermSet.from_list [InitialTerm.of_term t1; InitialTerm.of_term t2])))

let mul_terms (t1 : term) (t2 : term) : term =
  make_term (simplify_tt (InitialTerm.Times [InitialTerm.of_term t1; InitialTerm.of_term t2]))

(* SPINES *)

module Decide_Spines = struct 

(* right spines of a term *)
  let rspines (e : term) : TermSet.t =
    let open InitialTerm in 
    let rec sp (e : InitialTerm.t) : InitialTermSet.t =
      match e with
	| Dup  -> InitialTermSet.return One
	| Plus ts -> InitialTermSet.bind ts sp
	| Times l ->
          (match l with
            | [] -> InitialTermSet.empty
            | [d] -> sp d
            | d :: t ->
              let u = sp d in
              let v = sp (Times t) in
              let s = InitialTermSet.map (fun x -> Times (x :: t)) u in
              InitialTermSet.union s v)
	| Star d ->
          let s = sp d in
          InitialTermSet.map (fun x -> Times [x; e]) s
	| (Assg _ | Test _ | Not _ | Zero  | One  ) -> InitialTermSet.empty in
    InitialTermSet.fold (fun sp acc -> TermSet.add (make_term sp) acc) 
      (InitialTermSet.map simplify_tt (sp (InitialTerm.of_term e))) TermSet.empty
      
(* left spines of a term *)
  let lspines (e : term) : TermSet.t =
    let open InitialTerm in 
    let rec sp (e : InitialTerm.t) : InitialTermSet.t =
      match e with
	| Dup -> InitialTermSet.return One
	| Plus ts -> InitialTermSet.bind ts sp
	| Times l ->
          (match l with
            | [] -> InitialTermSet.empty
            | [d] -> sp d
            | d :: t ->
              let u = sp d in
              let v = sp (Times t) in
              let s = InitialTermSet.map (fun x -> Times [d; x]) v in
              InitialTermSet.union u s)
	| Star d ->
          let s = sp d in
          InitialTermSet.map (fun x -> Times [e; x]) s
	| (Assg _ | Test _ | Not _ | Zero | One) -> InitialTermSet.empty in
    
    InitialTermSet.fold (fun sp acc -> TermSet.add (make_term sp) acc)
	(InitialTermSet.map simplify_tt (sp (InitialTerm.of_term e))) TermSet.empty

      
  let lrspines (e : term) : TermSet.t =
    let open InitialTerm in 
    let rec lrspines e = 
      match e with
	| Dup -> InitialTermSet.return (Times [One; One])
	| Plus ts -> InitialTermSet.bind ts lrspines
	| Times l ->
	  (match l with
	    | [] -> InitialTermSet.empty
	    | [d] -> lrspines d
	    | d :: t ->
	      let u = lrspines d in
	      let v = lrspines (Times t) in
	      let f x = match x with
		| Times [l;r] -> Times [l; simplify_tt (Times (r :: t))]
		| _ -> failwith "lrspines 1" in
	      let r = InitialTermSet.map f u in
	      let g x = match x with
		| Times [l;r] -> Times [simplify_tt (Times [d;l]); r]
		| _ -> failwith "lrspines 2" in
	      let s = InitialTermSet.map g v in
	      InitialTermSet.union r s)
	| Star d ->
	  let s = lrspines d in
	  let f x = match x with
	    | Times [l;r] -> Times [simplify_tt (Times [e;l]); 
				    simplify_tt (Times [r;e])]
	    | _ -> failwith "lrspines 3" in
	  InitialTermSet.map f s
	| (Assg _ | Test _ | Not _ | Zero | One) -> InitialTermSet.empty
    in InitialTermSet.fold (fun x -> TermSet.add (make_term x)) (lrspines (InitialTerm.of_term e)) TermSet.empty
	
  (* get all lrspines of e and all lrspines of rspines of e *)
  let allLRspines (e : term) : (term, TermSet.t) Hashtbl.t =
    let allLR = TermSet.add e (rspines e) in
    let h = Hashtbl.create 11 in
    let f d = Hashtbl.add h d (lrspines d) in
    TermSet.iter f allLR; h
      
  (* (* remove dups of lspines *)                                            *)
  (* let remove_dups_from_Lspines (h : (term, TermSet.t) Hashtbl.t) : unit = *)
  (*   let f x = match x with                                                *)
  (*   | Times [l;r] -> Times [Decide_Ast.zero_dups l; r]                           *)
  (*   | _ -> failwith "remove_dups_from_Lspines" in                         *)
  (*   let g ts = TermSet.map f ts in                                        *)
      
  let display_lrspines (ts : TermSet.t) : unit =
    let f x = match x with
      | Times (_,[l;r]) -> Printf.printf "%s ||| %s\n" 
	(Term.to_string l) (Term.to_string r)
      | _ -> failwith "display_lrspines" in
    TermSet.iter f ts
      
end  

let hits = ref 0 
let misses = ref 1 

module TermMap = Map.Make(Term)

let memoize (f : Term.t -> 'a) = 
  let hash = Hashtbl.create 0 in 
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


let memoize_on_arg2 f = 
  let hash = Hashtbl.create 0 in 
  (fun a b -> 
    try let ret = Hashtbl.find hash b in
	(hits := !hits + 1;
	 ret)
    with Not_found -> 
      (misses := !misses + 1;
       let ret = f a b in 
       Hashtbl.replace hash b ret;
       ret
      ))
