open Util
open Ast
open Term
open Base

let universe (f : formula) : S.t =
  let univ = Ast.values_in_formula f in
  let g univ x = S.add x "<other>" univ in
  List.fold_left g univ (S.keys univ)
  
(* remove assignments where preceding test is a singleton with the same value *)
let remove_redundant_assgs ((t,p) : base) : base =
  let f x v = not (S.size x t = 1 && S.contains_value x v t) in
  let p = S.filter f p in (t,p)
  
let remove_all_redundant_assgs : B.t -> B.t =
  B.map remove_redundant_assgs
  
(* break out a base into atoms *)
let breakout_one (x : S.key) ((t,p) : base) : B.t =
  if S.size x t <= 1 then B.singleton (t,p)
  else let s = S.remove_all x t in
  S.fold_key (fun v -> B.add (S.add x v s, p)) x t B.empty
  
(* this can be made more efficient using fold *)
let breakout_more (x : S.key) (s : B.t) : B.t =
  let s = B.elements s in
  let s = List.map (breakout_one x) s in
  B.union_list s
  
let breakout (s : B.t) : B.t =
  let k = B.elements s in
  let k = List.map (fun (t,_) -> S.keys t) k in
  let k = remove_duplicates (List.concat k) in
  let r = List.fold_left (fun t x -> breakout_more x t) s k in
  remove_all_redundant_assgs r
 
let fillout (univ : S.t) (t : S.t) : S.t =
  let diff = S.filter (fun k v -> not (S.contains_key k t)) univ in
  S.union t diff
  
let fillout_base (univ : S.t) ((t,p) : base) : base =
  (fillout univ t, p)
  
let fillout_baseset (univ : S.t) : B.t -> B.t =
  B.map (fillout_base univ)
  
(* sanity check - check that all sets in t are singletons *)
(* and all are there *)  
let reduced (univ : S.t) (t : S.t) : unit =
  let f k = if S.contains_key k t && S.size k t = 1 then ()
            else failwith "Not reduced" in
  List.iter f (S.keys univ)
  
(* parameter univ is the universe *)
let rec normalize (univ : S.t) (t : term) : B.t =
  (* to negate a test x=v, we allow x to have any value except v *)
  let negate x v =
    (* sanity check *)
    if not (S.contains_value x v univ)
    then failwith (Printf.sprintf "Unknown variable/value %s/%s" x v);
    let s = S.fold_key (S.add x) x univ S.empty in
    B.singleton (S.remove x v s, S.empty) in
  let t = Ast.zero_dups t in (* may only normalize dup-free terms *)
  let t = Ast.deMorgan t in
  match t with 
  | Assg (x,v) ->
      let h1 = S.empty in
      let h2 = S.add x v (S.empty) in
      B.singleton (h1,h2)
  | Test (x,v) ->
      let h1 = S.add x v (S.empty) in
      let h2 = S.empty in
      B.singleton (h1,h2)
  | Dup -> failwith "Cannot normalize a term with dups"
  | Plus x ->
      let s = List.map (normalize univ) (TermSet.elements x) in
      B.union_list s
  | Times x ->
      let s = List.map (normalize univ) x in
      List.fold_left B.mult (B.singleton (S.empty,S.empty)) s
  | Not x -> begin
      match x with
      | Zero -> B.singleton (S.empty,S.empty)
      | One -> B.empty
      | Test (x,v) -> negate x v
      | _ -> failwith "De Morgan law should have been applied"
    end
  | Star x ->
      let s = normalize univ x in
      let s1 = B.add (S.empty,S.empty) s in
      let rec f (s : B.t) (r : B.t) : B.t =
        if B.equal s r then s
        else f (B.mult s s) s in
      f (B.mult s1 s1) s1
  | Zero -> B.empty
  | One -> B.singleton (S.empty,S.empty)
