open Util
open Ast

module S = StringSetMap
type ssm_pair = S.t * S.t

(* universe tells what values in the formula are associated with each variable *)
    (* ☃ is included with each variable to represent values not mentioned *)
let universe (f : formula) : S.t =
  let univ = Ast.values_in_formula f in
  let g u x = S.add x "☃" u in
  List.fold_left g univ (S.keys univ)


let ssm_to_string (t : S.t) (op : ('a -> 'b -> 'c, unit, string) format) : string =
  let s = S.bindings t in
  let f (x,a) = Printf.sprintf op x (String.concat "," a) in
  String.concat ";" (List.map f s)

let ssm_pair_to_string ((t,p) : ssm_pair) : string =
  Printf.sprintf "[%s,%s]" (ssm_to_string t "%s={%s}") (ssm_to_string p "%s:=%s")

module B = struct
  include Set.Make (struct
    type t = S.t * S.t
    let compare ((t1,p1) : t) ((t2,p2) : t) : int =
      let c = S.compare t1 t2 in
      if c = 0 then S.compare p1 p2 else c
  end)
  let to_string (bs : t) : string =
    String.concat " " (fold (fun x t -> ssm_pair_to_string x :: t) bs [])
    
  let map (f : ssm_pair -> ssm_pair) (bs : t) : t =
    fold (fun b -> add (f b)) bs empty
      
  let union_list (r : t list) : t =
    List.fold_left union empty r
    
  let map (f : ssm_pair -> ssm_pair) (s : t) : t =
    fold (fun b r -> add (f b) r) s empty


end

let fillout (univ : S.t) (t : S.t) : S.t =
  let diff = S.filter (fun k v -> not (S.contains_key k t)) univ in
  S.union t diff
  
let fillout_ssm_pair (univ : S.t) ((t,p) : ssm_pair) : ssm_pair=
  (fillout univ t, p)
  
let fillout_ssm_pairset (univ : S.t) : B.t -> B.t =
  B.map (fillout_ssm_pair univ)

let reduced (univ : S.t) (t : S.t) : unit =
  let f k = if S.contains_key k t && S.size k t = 1 then ()
    else failwith "Not reduced" in
  List.iter f (S.keys univ)

(* check whether a reduced ssm_pair is <= another ssm_pair *)
let leq ((t1,p1) : ssm_pair) ((t2,p2) : ssm_pair) : bool =
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
  
(* check whether a reduced ssm_pair is <= a ssm_pair set *)
let leq_ssm_pairset (b : ssm_pair) (s : B.t) : bool =
  B.exists (leq b) s
  
(* Check whether a condition holds for all tuples in *)
(* A1 x ... x An, where t = (x1,A1),...,(xn,An) in (t,p). *)
let forall_atoms (f : ssm_pair -> bool) ((t,p) : ssm_pair) : bool =
  let bindings = S.bindings t in
  let rec forall (c : S.t) (s : (string * (string list)) list) : bool =
    match s with
      | [] -> f (c,p)
      | (x,a) :: r -> List.for_all (fun v -> forall (S.add x v c) r) a in
  forall S.empty bindings

