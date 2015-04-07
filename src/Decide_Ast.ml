open Core.Std
open Sexplib.Conv
open Decide_Util

module FieldMap = Map.Make(Field)

type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet with sexp, compare

let packet_to_string pkt = Printf.sprintf "[%s]"
    (String.concat ~sep:";"
       (FieldMap.fold pkt ~init:[]
          ~f:(fun ~key ~data acc -> (Printf.sprintf "%s := %s" (Field.to_string key) (Value.to_string data) :: acc))))

module PacketSet = struct
  module S = Set.Make (struct
      type t = packet with sexp, compare
    end)

  let max_pkts_num = ref None
  let max_pkts () : int = match !max_pkts_num with
    | Some n -> n
    | None -> let fields = !all_fields () in
      let values = !all_values () in
      let max = FieldSet.fold (fun f acc -> acc * ValueSet.cardinal (values f)) fields 1 in
      max_pkts_num := Some max;
      max

  type t =
    | Positive of S.t
    | Negative of S.t with sexp

  type elt = packet with sexp, compare

  let empty = Positive S.empty
  let all = Negative S.empty

  let universe () = let fields = !all_fields () in
    let values = !all_values () in
    FieldSet.fold (fun f acc -> S.fold acc ~f:(fun acc pkt -> S.union acc (ValueSet.fold (fun v pkts -> S.add pkts (FieldMap.add pkt ~key:f ~data:v)) (values f) S.empty)) ~init:S.empty) fields (S.singleton (FieldMap.empty))
      
  let singleton a = Positive (S.singleton a)

  let complement t = match t with
    | Positive s -> Negative s
    | Negative s -> Positive s

  let length t = match t with
    | Positive s -> S.length s
    | Negative s -> max_pkts () - S.length s

  let is_empty t = match t with
    | Positive s -> S.is_empty s
    | Negative s -> max_pkts () = S.length s

  let mem t a = match t with
    | Positive s -> S.mem s a
    | Negative s -> not (S.mem s a)

  let add t a = match t with
    | Positive s -> Positive (S.add s a)
    | Negative s -> Negative (S.remove s a)

  let remove t a = match t with
    | Positive s -> Positive (S.remove s a)
    | Negative s -> Negative (S.add s a)

  let union t t' = match t,t' with
    | Positive s, Positive s' -> Positive (S.union s s')
    | Negative s, Negative s' -> Negative (S.inter s s')
    | Positive s, Negative s' -> Negative (S.diff s' s)
    | Negative s, Positive s' -> Negative (S.diff s s')

  let union_list ts = List.fold ts ~f:union ~init:empty

  let subset t t' = match t, t' with
    | Positive s, Positive s' -> S.subset s s'
    | Negative s, Negative s' -> S.subset s' s
    | _,_ -> failwith "NYI: PacketSet.subset (mixed Negative/Positive)"

  let inter t t' = match t,t' with
    | Positive s, Positive s' -> Positive (S.inter s s')
    | Negative s, Negative s' -> Negative (S.union s s')
    | Positive s, Negative s' -> Positive (S.diff s s')
    | Negative s, Positive s' -> Positive (S.diff s' s)

  let diff t t' = match t,t' with
    | Positive s, Positive s' -> Positive (S.diff s s')
    | t, Negative s' -> inter t (Positive s')
    | Negative s, Positive s' -> Negative (S.union s s')

  let equal t t' = if length t <> length t'
    then false
    else match t, t' with
      | Positive s, Positive s' -> S.equal s s'
      | Negative s, Negative s' -> S.equal s s'
      (* 
       Thm: if |A| = |B|, then A = B iff A-B = {} or B-A = {} To avoid
       constructing a bigger set, we always subtract the negative set
       from the positive one 
      *)
      | Negative s, Positive s' -> is_empty (diff t' t)
      | Positive s, Negative s' -> is_empty (diff t t')

  let exists t ~f = match t with
    | Positive s -> S.exists s ~f
    | Negative s -> failwith "NYI: PacketSet.exists (Negative)"

  let for_all t ~f = match t with
    | Positive s -> S.for_all s ~f
    | Negative s -> failwith "NYI: PacketSet.for_all (Negative)"

  let count t ~f = match t with
    | Positive s -> S.count s ~f
    | Negative s -> failwith "NYI: PacketSet.count (Negative)"

  let map t ~f = match t with
    | Positive s -> Positive (S.map s ~f)
    | Negative s -> failwith "NYI: PacketSet.map (Negative)"

  let filter_map t ~f = match t with
    | Positive s -> Positive (S.filter_map s ~f)
    | Negative s -> failwith "NYI: PacketSet.filter_map (Negative)"

  let filter t ~f = match t with
    | Positive s -> Positive (S.filter s ~f)
    | Negative s -> failwith "NYI: PacketSet.filter (Negative)"

  let fold t ~init ~f = match t with
    | Positive s -> S.fold s ~f ~init
    | Negative s -> S.fold (S.diff (universe ()) s) ~f ~init

  let elements t = match t with
    | Positive s -> S.elements s
    | Negative s -> failwith "NYI: PacketSet.elements (Negative)"

  let compare t t' = if equal t t' then 0 else -1
end

let point_to_string (pkt1, pkt2) = Printf.sprintf "(%s,%s)" (packet_to_string pkt1) (packet_to_string pkt2)

module rec TermBase : sig
  type t = term HashCons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSetBase.t
    | Times of t list
    | Intersection of TermSetBase.t
    | Not of t
    | Complement of t
    | Star of t
    | Zero 
    | One with compare, sexp
end = struct
  type t = term HashCons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSetBase.t
    | Times of t list
    | Intersection of TermSetBase.t
    | Not of t
    | Complement of t
    | Star of t
    | Zero 
    | One with compare, sexp

end and TermSetBase : sig
  include Set.S with type Elt.t = TermBase.t
end = Set.Make (struct
      type t = TermBase.t with compare, sexp
    end)

module Term (* : sig *)
(*   type t = TermBase.t with sexp, compare *)
(*   type term = TermBase.term with sexp, compare *)
(*   val compare_ab : t -> point -> bool *)
(*   val eval : t -> packet -> PacketSet.t *)
(*   val to_string : t -> string *)
(*   val assg : Field.t -> Value.t -> t *)
(*   val test : Field.t -> Value.t -> t *)
(*   val dup : t *)
(*   val plus : TermSet.t -> t *)
(*   val times : t list -> t     *)
(*   val not : t -> t *)
(*   val star : t -> t *)
(*   val zero : t *)
(*   val one : t *)
(* end *) = struct
  include TermBase
  open HashCons
  let rec eval (t : TermBase.t) (pkt : packet) = match t.node with
    | Assg (f,v) -> PacketSet.singleton (FieldMap.add pkt ~key:f ~data:v)
    | Test (f,v) -> begin match FieldMap.find pkt f with
        | Some v' -> if v' = v
          then PacketSet.singleton pkt
          else PacketSet.empty
        | None -> PacketSet.empty
      end
    | Dup -> raise (Failure "t must be dup-free")  
    | Plus ts -> TermSetBase.fold ts ~f:(fun acc t -> PacketSet.union (eval t pkt) acc) ~init:PacketSet.empty
    | Times ts -> List.fold ts ~init:(PacketSet.singleton pkt) ~f:(fun accum t ->
        PacketSet.fold accum ~init:PacketSet.empty ~f:(fun acc pkt -> PacketSet.union acc (eval t pkt)))
    | Intersection ts -> TermSetBase.fold ts ~f:(fun acc t -> PacketSet.inter (eval t pkt) acc) ~init:(eval (TermSetBase.choose_exn ts) pkt)
    | Not t -> let ret = eval t pkt in
      begin
        match PacketSet.length ret with
        | 0 -> PacketSet.singleton pkt
        | 1 -> PacketSet.empty
        | _ -> raise (Failure "Negation of a non-predicate")
      end
    | Complement t -> PacketSet.complement (eval t pkt)
    (* TODO: Copy fixpoint code from Frenetic *)
    | Star t -> raise (Failure "NYI")
    | Zero -> PacketSet.empty
    | One -> PacketSet.singleton pkt

  let compare_ab t point =
    let input,output = point in
    PacketSet.exists (eval t input) ~f:(FieldMap.equal Value.equal output)
      
  let rec to_string (t : t) : string =
    let out_precedence (t : t) : int =
      match t.node with
        | Plus _ -> 0
        | Times _ -> 1
        | Intersection _ -> 2
        | Not _ -> 3
        | Star _ -> 4
        | _ -> 4 in
    let protect (u:t) : string =
      let s = to_string u in
      if out_precedence t <= out_precedence u then s
      else Printf.sprintf "(%s)" s in 
    let assoc_to_string (op : string) (init : string) (s : string list) : string = 
      match s with
        | [] -> init
        | _ -> String.concat ~sep:op s in
    match t.node with
      | Assg (f,v) -> 
        Printf.sprintf "%s:=%s" 
          (Field.to_string f) (Value.to_string v)
      | Test (f,v) -> 
        Printf.sprintf "%s=%s" 
          (Field.to_string f) (Value.to_string v)
      | Dup -> 
        "dup"
      | Plus (ts) -> 
        assoc_to_string " + " "drop" 
          (List.map ~f:protect (TermSetBase.elements ts))
      | Intersection (ts) ->
        assoc_to_string " ^ " "WRONG_EMPTY_INTERSECTION" 
          (List.map ~f:protect (TermSetBase.elements ts))
      | Times (ts) -> 
        assoc_to_string ";" "id" (List.map ~f:protect ts)
      | Not (t) -> 
        "~" ^ (protect t)
      | Star (t) -> 
        (protect t) ^ "*"
      | Complement (t) ->
        "!" ^ (protect t)
      | Zero -> 
        "drop"
      | One -> 
        "id"

  module H = Make(struct
      type t = TermBase.term with sexp, compare
      let equal a b = compare a b = 0
      let hash = Hashtbl.hash
    end)

  let hashtbl = H.create 100
  let assg f v = H.hashcons hashtbl (Assg (f,v))
  let test f v = H.hashcons hashtbl (Test (f,v))
  let dup = H.hashcons hashtbl Dup
  let plus ts = H.hashcons hashtbl (Plus ts)
  let times ts = H.hashcons hashtbl (Times (List.fold_right ts ~init:[] ~f:(fun x acc -> match x.node with
      | One -> acc
      | Times ts' -> ts' @ acc
      | _ -> x :: acc)))
  let intersection ts = H.hashcons hashtbl (Intersection ts)
  let complement t = H.hashcons hashtbl (Complement t)
  let not t = H.hashcons hashtbl (Not t)
  let star t = H.hashcons hashtbl (Star t)
  let zero = H.hashcons hashtbl Zero
  let one = H.hashcons hashtbl One

  module UnivMap = SetMapF(Field)(Value)
  (* Collect the possible values of each variable *)
  let values (t : TermBase.t) : UnivMap.t =
    let rec collect (m : UnivMap.t) (t : TermBase.t) : UnivMap.t =
      match t.node with
	| (Assg (x,v) | Test (x,v)) -> UnivMap.add x v m
        | Plus s -> TermSetBase.fold s ~init:m ~f:collect
        | Intersection s -> TermSetBase.fold s ~init:m ~f:collect
	| Times s -> List.fold_right s ~init:m ~f:(fun a b -> collect b a)
	| (Not x | Star x | Complement x) -> collect m x
	| (Dup  | Zero  | One ) -> m in
    collect UnivMap.empty t

  let equal t1 t2 = compare t1 t2 = 0
  let rec size t = 
    match t.node with 
      | Assg(f,v) -> 
        1
      | Test(f,v) -> 
        1
      | Dup -> 
        1
      | Plus ts -> 
        TermSetBase.fold ts
          ~f:(fun n ti -> (size ti) + n)
          ~init:1
      | Times ts -> 
        List.fold_left ts 
          ~f:(fun n ti -> n + (size ti))
          ~init:1
      | Intersection ts -> 
        TermSetBase.fold ts
          ~f:(fun n ti -> (size ti) + n)
          ~init:1
      | (Not t | Star t | Complement t) -> 1 + size t
      | Zero -> 1
      | One -> 1                    
end

module TermSet = struct
  include TermSetBase
  let to_string ts = Printf.sprintf "{%s}" (String.concat ~sep:", " (List.map (elements ts) Term.to_string))
end

module Path = struct

  type regex =
      Const of Value.t
    | Any
    | Sequence of regex * regex
    | Union of regex * regex
    | Intersection of regex * regex
    | Comp of regex
    | Star of regex
    | Empty
    | EmptySet with sexp, compare

  type t =
      RegPol of (Field.t * Value.t) * regex
    | RegUnion of t * t
    | RegInter of t * t with sexp, compare

  let (<+>) a b = RegUnion(a,b)
  let (<*>) a b = RegInter(a,b)

  let (&&) a b = Intersection(a,b)
  let (||) a b = Union(a,b)
  let (<.>) a b = Sequence(a,b)

  let rec regex_to_string reg = match reg with
    | Const(h) -> Printf.sprintf "%s" (Value.to_string h)
    | Any -> "Any"
    | Empty -> "Empty"
    | EmptySet -> "{}"
    | Comp r -> Printf.sprintf "not (%s)" (regex_to_string r)
    | Sequence(r1, r2) -> Printf.sprintf "( %s <.> %s )" (regex_to_string r1) (regex_to_string r2)
    | Union(r1, r2) -> Printf.sprintf "( %s <||> %s )" (regex_to_string r1) (regex_to_string r2)
    | Intersection(r1, r2) -> Printf.sprintf "( %s <&&> %s )" (regex_to_string r1) (regex_to_string r2)
    | Star r -> Printf.sprintf "(%s)*" (regex_to_string r)

  let rec t_to_string regPol = match regPol with
  | RegPol((f,v), r) -> Printf.sprintf "%s=%s => %s"
          (Field.to_string f) (Value.to_string v) (regex_to_string r)
  | RegUnion(t1, t2) -> Printf.sprintf "(%s <+> %s)" (t_to_string t1) (t_to_string t2)
  | RegInter(t1, t2) -> Printf.sprintf "(%s <*> %s)" (t_to_string t1) (t_to_string t2)

  let compare = compare_t

  let sw_hdr = Field.of_string "sw"

  let rec translate_regex r = match r with
    | Const(h) -> Term.(times [test sw_hdr h;
                               plus (ValueSet.fold (fun x acc -> TermSet.add acc (assg sw_hdr x)) (!all_values () sw_hdr) TermSet.empty);
                               dup])
    | Any -> Term.(times [plus (ValueSet.fold (fun x acc -> TermSet.add acc (assg sw_hdr x)) (!all_values () sw_hdr) TermSet.empty);
                          dup])
    | Empty -> Term.one
    | EmptySet -> Term.zero
    | Sequence(r1,r2) -> Term.times [translate_regex r1; translate_regex r2]
    | Union(r1,r2) -> Term.plus (TermSet.of_list [translate_regex r1; translate_regex r2])
    | Intersection(r1,r2) -> Term.intersection (TermSet.of_list [translate_regex r1; translate_regex r2])
    | Comp r -> Term.complement (translate_regex r)
    | Star r -> Term.star (translate_regex r)
      
  let rec translate t = match t with
    | RegPol((f,v), r) -> Term.(times [assg f v; translate_regex r])

  module UnivMap = SetMapF(Field)(Value)
  (* Collect the possible values of each variable *)
  let rec values (t : t) : UnivMap.t =
    let rec collect (m : UnivMap.t) (r : regex) : UnivMap.t =
      match r with
	| Const h -> UnivMap.add sw_hdr h m
        | (Union(r1,r2) | Intersection(r1,r2) | Sequence(r1,r2))-> collect (collect m r1) r2
	| (Comp x | Star x) -> collect m x
        | (Any  | EmptySet  | Empty ) -> m in
    match t with
    | RegPol((h,v), r) -> collect (UnivMap.add h v UnivMap.empty) r
    | (RegInter(r1,r2) | RegUnion(r1,r2)) -> UnivMap.union (values r1) (values r2)
  
end

module Formula = struct
  type t =
    | Neq of Term.t * Term.t
    | Eq of Term.t * Term.t
    | Le of Term.t * Term.t
    | Sat of Term.t * Path.t
        
  let make_eq (t1:Term.t) (t2:Term.t) : t =
    Eq (t1,t2)

  let make_neq (t1:Term.t) (t2:Term.t) : t =
    Neq (t1,t2)

  let make_le (t1:Term.t) (t2:Term.t) : t =
    Le (t1,t2)

  let make_sat (t:Term.t) (p:Path.t) : t =
    Sat (t,p)

  let to_string (f:t) : string =
    match f with
      | Eq (s,t) ->
        Printf.sprintf "%s == %s"
          (Term.to_string s) (Term.to_string t)
      | Neq (s,t) ->
        Printf.sprintf "%s != %s"
          (Term.to_string s) (Term.to_string t)
      | Le (s,t) ->
        Printf.sprintf "%s <= %s"
          (Term.to_string s) (Term.to_string t)
      | Sat (t,p) ->
        Printf.sprintf "%s |= %s"
          (Term.to_string t) (Path.t_to_string p)

  let compare (f1:t) (f2:t) : int =
    match f1,f2 with
    | Neq(s1,t1), Neq(s2,t2) ->
      let cmp = Term.compare s1 s2 in
      if cmp <> 0 then cmp
      else Term.compare t1 t2
    | Eq(s1,t1), Eq(s2,t2) ->
      let cmp = Term.compare s1 s2 in
      if cmp <> 0 then cmp
      else Term.compare t1 t2
    | Le(s1,t1), Le(s2,t2) ->
      let cmp = Term.compare s1 s2 in
      if cmp <> 0 then cmp
      else Term.compare t1 t2
    | Sat(t1,p1), Sat(t2,p2) ->
      let cmp = Term.compare t1 t2 in
      if cmp <> 0 then cmp
      else Path.compare p1 p2
    | Eq _, _ -> -1
    | _ -> 1

  let equal (f1:t) (f2:t) : bool =
    compare f1 f2 = 0

  let terms (f:t) =
    match f with
    | Neq (s,t) -> (s,t)
    | Eq (s,t) -> (s,t)
    | Le (s,t) -> (s,t)
end
