
open Util
exception Empty

(***********************************************
 * syntax
 ***********************************************)

type id = string

(* character encoding *)
let utf8 = ref false
  
module rec Term : sig
  type t =
  | Assg of id * string
  | Test of id * string
  | Dup
  | Plus of TS.t
  | Times of t list
  | Not of t
  | Star of t
  | Zero
  | One
end = Term
and TS : (Set.S with type elt = Term.t) = Set.Make (struct
  type t = Term.t
  let compare = Pervasives.compare
end)

module TermSet = struct
  include TS
  let map (f : Term.t -> Term.t) (t : t) : t =
    fold (fun x -> add (f x)) t empty
  let from_list (tl : Term.t list) : t =
    List.fold_right add tl empty
end

open Term
type term = Term.t
 
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
  | [] -> ident
  | _ -> String.concat op s

let rec term_to_string (t : term) : string =
  (* parenthesize as dictated by surrounding precedence *)
  let protect (x : term) : string =
    let s = term_to_string x in
    if out_precedence t <= out_precedence x then s else "(" ^ s ^ ")" in
    match t with
    | Assg (var, value) -> var ^ ":=" ^ value
    | Test (var, value) -> var ^ "=" ^ value
    | Dup -> "dup"
    | Plus x -> assoc_to_string " + " "0" (List.map protect (TermSet.elements x))
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
    | (Assg _ | Test _ | Zero | One | Dup) -> ()
    | Plus x -> siblings 0 pos (TermSet.elements x)
    | Times x -> siblings 0 pos x
    | Not x -> subterms (0 :: pos) x
    | Star x -> subterms (0 :: pos) x in
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

module S = StringSetMap

let terms_in_formula (f : formula) =
  match f with (Eq (s,t) | Le (s,t)) -> (s,t)

let rec is_test (t : term) : bool =
  match t with
  | Assg _ -> false
  | Test _ -> true
  | Dup -> false
  | Times x -> List.for_all is_test x
  | Plus x -> TermSet.for_all is_test x
  | Not x -> is_test x || failwith "May not negate an action"
  | Star x -> is_test x
  | (Zero | One) -> true

let rec vars_in_term (t : term) : id list =
  match t with
  | (Assg (x,_) | Test(x,_)) -> [x]
  | Times x -> List.concat (List.map vars_in_term x)
  | Plus x -> List.concat (List.map vars_in_term (TermSet.elements x))
  | (Not x | Star x) -> vars_in_term x
  | _ -> []

let vars_in_formula (f : formula) : id list =
  let (s,t) = terms_in_formula f in
  remove_duplicates (List.append (vars_in_term s) (vars_in_term t))

(* Collect the possible values of each variable *)
let values_in_term (t : term) : S.t =
  let rec collect (t : term) (h : S.t) : S.t =
  match t with 
  | Assg (x,v) -> S.add x v h
  | Test (x,v) -> S.add x v h
  | Dup -> h
  | Plus s -> TermSet.fold collect s h
  | Times s -> List.fold_right collect s h
  | Not x -> collect x h
  | Star x -> collect x h
  | Zero -> h
  | One -> h in
  collect t S.empty

let values_in_formula (f : formula) : S.t =
  let (s,t) = terms_in_formula f in
  S.union (values_in_term s) (values_in_term t)

(***********************************************
 * simplify
 ***********************************************)

(* flatten terms *)
let flatten_sum (t : term list) : term =
  let f x = match x with Plus v -> (TermSet.elements v) | Zero -> [] | _ -> [x] in
  let t1 = List.concat (List.map f t) in
  let t2 = TermSet.from_list t1 in
  match TermSet.elements t2 with [] -> Zero | [x] -> x | _ -> Plus t2
    
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
  let t1 = match t with
  | Plus x -> flatten_sum (List.filter (fun s -> not (is_test s)) (TermSet.elements x))
  | _ -> t in
  if is_test t1 then One
  else match t1 with
  | Star _ -> t1
  | _ -> Star t1
    
let rec simplify (t : term) : term =
  match t with
  | Plus x -> flatten_sum (List.map simplify (TermSet.elements x))
  | Times x -> flatten_product (List.map simplify x)
  | Not x -> flatten_not (simplify x)
  | Star x -> flatten_star (simplify x)
  | _ -> t

let simplify_formula (e : formula) : formula =
  match e with
  | Eq (s,t) -> Eq (simplify s, simplify t)
  | Le (s,t) -> Le (simplify s, simplify t)

(* set dups to 0 *)
let zero_dups (t : term) : term =
  let rec zero t =
    match t with 
    | (Assg _ | Test _ | Zero | One) -> t
    | Dup -> Zero
    | Plus x -> Plus (TermSet.map zero x)
    | Times x -> Times (List.map zero x)
    | Not x -> Not (zero x)
    | Star x -> Star (zero x) in
  simplify (zero t)

(* apply De Morgan laws to push negations down to the leaves *)
let deMorgan (t : term) : term =
  let rec dM (t : term) : term =
    let f x = dM (Not x) in
    match t with 
    | (Assg _ | Test _ | Zero | One | Dup) -> t
    | Plus x -> Plus (TermSet.map dM x)
    | Times x -> Times (List.map dM x)
    | Not (Not x) -> dM x
    | Not (Plus s) -> Times (List.map f (TermSet.elements s))
    | Not (Times s) -> Plus (TermSet.from_list (List.map f s))
    | Not (Star x) ->
      if is_test x then Zero
      else failwith "May not negate an action"
    | Not Zero -> One
    | Not One -> Zero
    | Not _ -> t
    | Star x -> Star (dM x) in
  simplify (dM t)
