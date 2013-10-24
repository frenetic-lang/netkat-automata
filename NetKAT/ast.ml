
(* open Util *)

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
      Assg _ -> false
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

  let vars_in_formula (ce : formula) : id list =
		match ce with
		| (Eq (s,t) | Le (s,t)) -> Util.removeDuplicates (List.append (vars_in_term s) (vars_in_term t))

(* (*  let substVariables (s : substitution) : id list =               *)                      *)
(* (*    Subst.fold (fun id term list -> id :: list) s []              *)                      *)

  (* (* flatten terms *)                                                                       *)
  (* let rec flatten (t : term) : term =                                                       *)
  (*   match t with                                                                            *)
  (*     Plus x ->                                                                             *)
  (*       let y = List.map flatten x in                                                       *)
  (*       let z = List.concat (List.map (fun u -> match u with Plus v -> v | _ -> [u]) y) in  *)
  (*       (match z with [] -> Zero | [x] -> x | _ -> Plus z)                                  *)
  (*   | Times x ->                                                                            *)
  (*       let y = List.map flatten x in                                                       *)
  (*       let z = List.concat (List.map (fun u -> match u with Times v -> v | _ -> [u]) y) in *)
  (*       (match z with [] -> One | [x] -> x | _ -> Times z)                                  *)
  (*   | Not x -> Not (flatten x)                                                              *)
  (*   | Star x -> Star (flatten x)                                                            *)
  (*   | _ -> t                                                                                *)

  (* let rec flatten_formula (e : formula) : formula =                                         *)
  (*   match e with                                                                            *)
  (*   | Eq (s,t) -> Eq (flatten s, flatten t)                                                 *)
  (*   | Le (s,t) -> Le (flatten s, flatten t)                                                 *)
    
(***********************************************
 * simplify
 ***********************************************)

  (* convert empty sums and products to 0 and 1, resp,
   * and sums and products of one element to that element,
   * combine adjacent sums and products *)
  let rec simplifyLite (t : term) : term =
    match t with
    | Plus s ->
			(match (Termset.elements s) with
			| [] -> Zero
      | [x] -> simplifyLite x
      | y ->
        let y' = List.map simplifyLite y in
        let f = function (Plus z) -> fun zz -> (Termset.elements z) @ zz
                       | x -> fun zz -> x :: zz in
        Plus (termset_from_list (List.fold_right f y' [])))
    | Times [] -> One
    | Times [t] -> simplifyLite t
    | Times y ->
        let y' = List.map simplifyLite y in
        let f = function (Times z) -> fun zz -> z @ zz
                       | x -> fun zz -> x :: zz in
        Times (List.fold_right f y' [])
    | Not x -> Not (simplifyLite x)
    | Star x -> Star (simplifyLite x)
    | _ -> t

  let rec simplify (t : term) : term =
    match t with
      Plus x ->
				let x0 = Termset.elements x in
        let x1 = List.map simplify x0 in
        let x2 = List.filter (function Zero -> false | _ -> true) x1 in
        let x3 = Util.removeDuplicates x2 in
        simplifyLite (Plus (termset_from_list x3))
    | Times x ->
        let x1 = List.map simplify x in
        let x2 = List.filter (function One -> false | _ -> true) x1 in
        let x3 = if List.mem Zero x2 then [] else x2 in
        simplifyLite (Times x3)
    | Not (Not x) -> simplify x
    | Not One -> Zero
    | Not Zero -> One
    | Not x -> Not (simplify x)
    | Star (Star x) -> simplify (Star x)
    | Star x ->
        let x' = simplify x
        in if is_test x' then One else Star x'
    | _ -> t

	let simplify_formula (e : formula) : formula =
		match e with
		| Eq (s,t) -> Eq (simplify s, simplify t)
		| Le (s,t) -> Le (simplify s, simplify t)

