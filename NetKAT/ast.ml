
open Util

(***********************************************
 * syntax
 ***********************************************)

type id = string

(* character encoding *)
let utf8 = ref false
	
module rec Term : sig
	  type term =
	  | Assg of id * string
	  | Test of id * string
	  | Plus of Termset.t
	  | Times of term list
	  | Not of term
	  | Star of term
	  | Zero
	  | One
end = Term
and Termset : (Set.S with type elt = Term.term) = Set.Make (struct
		type t = Term.term
		let compare = Pervasives.compare
end)
	
open Term
type term = Term.term
	
	let termset_from_list (tl : term list) : Termset.t =
		List.fold_right Termset.add tl Termset.empty
	
  (* type substitution = (id, term) Subst.t *)
  type formula = Eq of term * term | Le of term * term

(***********************************************
 * output
 ***********************************************)

  (* higher precedence binds tighter *)
  let out_precedence (t : term) : int =
    match t with
    | Plus _ -> 0
    | Times _ -> 1
    | Not _ -> 2
    | Star _ -> 3
    | _ -> 4 (* assignments and primitive tests *)

  let assoc_to_string (op : string) (ident : string) (s : string list) : string =
    match s with
      [] -> ident
    | _ -> String.concat op s

  let rec term_to_string (t : term) : string =
    (* parenthesize as dictated by surrounding precedence *)
    let protect (x : term) : string =
      let s = term_to_string x in
      if out_precedence t <= out_precedence x then s else "(" ^ s ^ ")" in
    match t with
    | Assg (var, value) -> var ^ ":=" ^ value
    | Test (var, value) -> var ^ "=" ^ value
    | Plus x -> assoc_to_string " + " "0" (List.map protect (Termset.elements x))
    | Times x -> assoc_to_string ";" "1" (List.map protect x)
    | Not x -> (if !utf8 then "¬" else "~") ^ (protect x)
    | Star x -> (protect x) ^ "*"
    | Zero -> "drop"
    | One -> "pass"

  let formula_to_string (e : formula) =
    match e with
		| Eq (s,t) -> term_to_string s ^ " == " ^ term_to_string t
		| Le (s,t) -> term_to_string s ^ " <= " ^ term_to_string t

(***********************************************
 * utilities
 ***********************************************)

  let rec is_test (t : term) : bool =
    match t with
    | Assg _ -> false
    | Test _ -> true
    | Times x -> List.for_all is_test x
    | Plus x -> Termset.for_all is_test x
    | Not x -> is_test x || failwith "May not have action under a negation"
    | Star x -> is_test x
    | (Zero | One) -> true

  let rec vars_in_term (t : term) : id list =
    match t with
    | (Assg (x,_) | Test(x,_)) -> [x]
    | Times x -> List.concat (List.map vars_in_term x)
    | Plus x -> List.concat (List.map vars_in_term (Termset.elements x))
    | (Not x | Star x) -> vars_in_term x
    | _ -> []

  let vars_in_formula (f : formula) : id list =
  	match f with
  	| (Eq (s,t) | Le (s,t)) ->
			removeDuplicates (List.append (vars_in_term s) (vars_in_term t))

  (* collects the possible values of each variable *)
  let rec values (h : (id, string) SetMap.t) (t : term) : unit =
    match t with
    | Assg (x,v) -> SetMap.add h x v
    | Test (x,v) -> SetMap.add h x v
    | Times x -> List.iter (values h) x
    | Plus x -> Termset.iter (values h) x
    | Not x -> values h x
    | Star x -> values h x
    | (Zero | One) -> ()

  let values_in_term (t : term) : (id, string) SetMap.t =
    let h = SetMap.make() in values h t; h

  let values_in_formula (f : formula) : (id, string) SetMap.t =
		match f with
		| (Eq (s,t) | Le (s,t)) ->
			let h = values_in_term s in values h t; h

(* (*  let substVariables (s : substitution) : id list =               *)                      *)
(* (*    Subst.fold (fun id term list -> id :: list) s []              *)                      *)

(***********************************************
 * simplify
 ***********************************************)

  (* flatten terms *)
  let flatten_sum (t : term list) : term =
    let f x = match x with Plus v -> (Termset.elements v) | Zero -> [] | _ -> [x] in
    let t1 = List.concat (List.map f t) in
    let t2 = termset_from_list t1 in
    match Termset.elements t2 with [] -> Zero | [x] -> x | _ -> Plus t2
    
  let flatten_product (t : term list) : term =
    let f x = match x with Times v -> v | One -> [] | _ -> [x] in
    let t1 = List.concat (List.map f t) in
    if List.exists (fun x -> match x with Zero -> true | _ -> false) t1 then Zero
    else match t1 with [] -> One | [x] -> x | _ -> Times t1
    
  let flatten_not (t : term) : term =
    match t with
    | Not y -> y
    | Zero -> One
    | One -> Zero
    | _ -> Not t
    
  let flatten_star (t : term) : term =
    if is_test t then One
    else match t with
    | Star _ -> t
    | _ -> Star t
    
  let rec simplify (t : term) : term =
    match t with
    | Plus x -> flatten_sum (List.map simplify (Termset.elements x))
    | Times x -> flatten_product (List.map simplify x)
		| Not x -> flatten_not (simplify x)
    | Star x -> flatten_star (simplify x)
    | _ -> t

	let simplify_formula (e : formula) : formula =
		match e with
		| Eq (s,t) -> Eq (simplify s, simplify t)
		| Le (s,t) -> Le (simplify s, simplify t)
