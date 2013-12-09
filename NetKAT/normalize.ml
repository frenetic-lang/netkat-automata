
open Util
open Ast
open Term

(* A base is a pair (t,p) of StringSetMaps, where t is a set of tests      *)
(* of the form x in A for a variable x and set of values A and each x      *)
(* occurs at most once in t, and p is a sequence of assignments x := n,    *)
(* where each x occurs at most once in p. The values in p are singletons.  *)

module S = StringSetMap
type base = S.t * S.t

let ssm_to_string (t : S.t) (op : ('a -> 'b -> 'c, unit, string) format) : string =
  let s = S.bindings t in
  let f (x,a) = Printf.sprintf op x (String.concat "," a) in
  String.concat ";" (List.map f s)

let base_to_string ((t,p) : base) : string =
  Printf.sprintf "[%s,%s]" (ssm_to_string t "%s={%s}") (ssm_to_string p "%s:=%s")

module BaseSet = struct
  include Set.Make (struct
    type t = base
    let compare ((t1,p1) : t) ((t2,p2) : t) : int =
      let c = S.compare t1 t2 in
      if c = 0 then S.compare p1 p2 else c
  end)

  let to_string (bs : t) : string =
    String.concat " " (fold (fun x t -> base_to_string x :: t) bs [])
    
  (* multiply two bases *)
  let mult_base ((t1,p1) : base) ((t2,p2) : base) : base option =
    (* Are all x:=n in p1 consistent with t2? *)
    let g x v (op_t : S.t option) : S.t option =
      let f t = if S.consis x v t then Some (S.remove_all x t) else None in
      Option.bind f op_t in
    let t2 = S.fold g p1 (Some t2) in
    if t2 = None then None else
    let t2 = Option.get t2 in
    (* construct new assignments *)
    let g x v p =
      if S.contains_key x p then p
      else S.add x v p in
    let p3 = S.fold g p1 p2 in
    (* construct new tests *)
    let f s x v t =
      if S.consis x v s then S.add x v t else t in
    let t3 = S.fold (f t1) t2 S.empty in
    let t3 = S.fold (f t2) t1 t3 in
    (* check if tests were consistent *)
    let k = remove_duplicates (List.append (S.keys t1) (S.keys t2)) in 
    if List.exists (fun x -> S.size x t3 = 0) k then None else
    Some (t3,p3)
  
  (* multiply BaseSets *)
  let mult (left : t) (right : t) : t =
    let f (x : base) (y : base) (r : t) : t =
      match mult_base x y with
      | Some z -> add z r
      | None -> r in
    let g (x : base) (r : t) : t = fold (f x) right r in
    fold g left empty
    
  let union_list (r : t list) : t =
    List.fold_left union empty r
    
  let map (f : base -> base) (s : t) : t =
    fold (fun b r -> add (f b) r) s empty
end

module B = BaseSet

let universe (f : formula) : S.t =
  let h = Ast.values_in_formula f in
  let g h x = S.add x "<other>" h in
  List.fold_left g h (S.keys h)
  
(* remove assignments where preceding test is a singleton with the same value *)
let remove_redundant_assgs ((t,p) : base) : base =
  let f x v r = if S.size x t = 1 && S.contains_value x v t then r else S.add x v r in
  let p = S.fold f p S.empty in (t,p)
  
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
  
(* parameter h is the universe *)
let rec normalize (h : S.t) (t : term) : B.t =
  (* to negate a test x=v, we allow x to have any value except v *)
  let negate x v =
    (* sanity check *)
    if not (S.contains_value x v h)
    then failwith (Printf.sprintf "Fatal error: Unknown variable/value %s/%s" x v);
    let s = S.fold_key (S.add x) x h S.empty in
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
  | Dup -> failwith "Fatal error: Cannot normalize a term with dups"
  | Plus x ->
      let s = List.map (normalize h) (TermSet.elements x) in
      B.union_list s
  | Times x ->
      let s = List.map (normalize h) x in
      List.fold_left B.mult (B.singleton (S.empty,S.empty)) s
  | Not x -> begin
      match x with
      | Zero -> B.singleton (S.empty,S.empty)
      | One -> B.empty
      | Test (x,v) -> negate x v
      | _ -> failwith "Fatal error: de Morgan law should have been applied"
    end
  | Star x ->
      let s = normalize h x in
      let s1 = B.add (S.empty,S.empty) s in
      let rec f (s : B.t) (r : B.t) : B.t =
        if B.equal s r then s
        else f (B.mult s s) s in
      f (B.mult s1 s1) s1
  | Zero -> B.empty
  | One -> B.singleton (S.empty,S.empty)

(* evaluate without dups for now *)
let eval (f : formula) : bool =
  let (s,t) = Ast.terms_in_formula f in
  let h = universe f in
  let s = breakout (normalize h s) in
  let t = breakout (normalize h t) in
  let s =
    match f with
    | Eq _ -> s
    | Le _ -> B.union s t in
  s = t

