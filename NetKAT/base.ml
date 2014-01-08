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
    
  let map (f : base -> base) (bs : t) : t =
    fold (fun b -> add (f b)) bs empty
    
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

let display_base (b : base) =
  Printf.printf "%s\n" (base_to_string b)
  
let display_baseset (m : B.t) =
  print_endline (B.to_string m)

let display_binding (k,l) =
  let s = String.concat "," l in
  Printf.printf "%s:{%s} " k s
