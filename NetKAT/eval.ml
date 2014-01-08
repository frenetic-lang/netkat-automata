open Ast
open Base
open Normalize

(* check whether a reduced base is <= another base *)
let leq ((t1,p1) : base) ((t2,p2) : base) : bool =
  S.for_all (fun k v -> S.consis k v t2) t1 &&
  (* For every assignment in p1, check consistency with p2 *)
  let f k v =
    S.contains_value k v p2 ||
    not (S.contains_key k p2) && S.consis k v t2 in
  S.for_all f p1 &&
  (* For every assignment in p2, check consistency with p1 *)
  let f k v =
    S.contains_value k v p1 ||
    not (S.contains_key k p1) && S.contains_value k v t1 in
  S.for_all f p2
  
(* check whether a reduced base is <= a base set *)
let leq_baseset (b : base) (s : B.t) : bool =
  B.exists (leq b) s
  
(* Check whether a condition holds for all tuples in      *)
(* A1 x ... x An, where t = (x1,A1),...,(xn,An) in (t,p). *)
let forall_atoms (f : base -> bool) ((t,p) : base) : bool =
  let bindings = S.bindings t in
  let rec forall (c : S.t) (s : (string * (string list)) list) : bool =
    match s with
    | [] -> f (c,p)
    | (x,a) :: r -> List.for_all (fun v -> forall (S.add x v c) r) a in
  forall S.empty bindings

let test_atomized_baseset (univ : S.t) (s : B.t) : unit =
  let f ((t,p) : base) : bool = reduced univ t; true in
  let g b = ignore (forall_atoms f (fillout_base univ b)) in
  B.iter g s

(* evaluate without dups for now *)
let eval (f : formula) : bool =
  let (s,t) = Ast.terms_in_formula f in
  let univ = universe f in
  let sn = normalize univ s in
  let tn = normalize univ t in
  let sf = fillout_baseset univ sn in
  let tf = fillout_baseset univ tn in
  let check_leq (rhs : B.t) ((t,p) : base) : bool =
    reduced univ t;
    leq_baseset (t,p) rhs in   
  match f with
  | Eq _ -> B.for_all (forall_atoms (check_leq tn)) sf && B.for_all (forall_atoms (check_leq sn)) tf
  | Le _ -> B.for_all (forall_atoms (check_leq tn)) sf
