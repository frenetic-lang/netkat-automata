open Core.Std
open Sexplib.Conv
open Decide_Util

module FieldMap = Map.Make(Field)

type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet with sexp, compare

module PacketSet = Set.Make (struct
    type t = packet with sexp, compare
  end)

let packet_to_string pkt = Printf.sprintf "[%s]"
    (String.concat ~sep:";"
       (FieldMap.fold pkt ~init:[]
          ~f:(fun ~key ~data acc -> (Printf.sprintf "%s := %s" (Field.to_string key) (Value.to_string data) :: acc))))

let point_to_string (pkt1, pkt2) = Printf.sprintf "(%s,%s)" (packet_to_string pkt1) (packet_to_string pkt2)

module rec TermBase : sig
  type t = term HashCons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSetBase.t
    | Times of t list 
    | Not of t
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
    | Not of t
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
    | Not t -> let ret = eval t pkt in
      begin
        match PacketSet.length ret with
        | 0 -> PacketSet.singleton pkt
        | 1 -> PacketSet.empty
        | _ -> raise (Failure "Negation of a non-predicate")
      end
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
        | Not _ -> 2
        | Star _ -> 3
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
      | Times (ts) -> 
        assoc_to_string ";" "id" (List.map ~f:protect ts)
      | Not (t) -> 
        "~" ^ (protect t)
      | Star (t) -> 
        (protect t) ^ "*"
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
  let zero = H.hashcons hashtbl Zero
  let one = H.hashcons hashtbl One
  let assg f v = H.hashcons hashtbl (Assg (f,v))
  let test f v = H.hashcons hashtbl (Test (f,v))
  let dup = H.hashcons hashtbl Dup
  let plus ts = H.hashcons hashtbl (Plus (TermSetBase.filter ts ~f:(fun x -> match x.node with
      | Zero -> false
      | _ -> true)))
  let times ts = H.hashcons hashtbl (Times (List.fold_right ts ~init:[] ~f:(fun x acc -> match x.node with
      | One -> acc
      | Zero -> [zero]
      | Times ts' -> ts' @ acc
      | _ -> x :: acc)))
  let not t = match t.node with
    | Zero -> one
    | One -> zero
    | _ -> H.hashcons hashtbl (Not t)
  let star t = match t.node with
    | Zero
    | One -> one
    | _ -> H.hashcons hashtbl (Star t)

  module UnivMap = SetMapF(Field)(Value)
  (* Collect the possible values of each variable *)
  let values (t : TermBase.t) : UnivMap.t =
    let rec collect (m : UnivMap.t) (t : TermBase.t) : UnivMap.t =
      match t.node with
	| (Assg (x,v) | Test (x,v)) -> UnivMap.add x v m
	| Plus s -> TermSetBase.fold s ~init:m ~f:collect
	| Times s -> List.fold_right s ~init:m ~f:(fun a b -> collect b a)
	| (Not x | Star x) -> collect m x
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
      | Not t -> 1 + size t
      | Star t -> 1 + size t
      | Zero -> 1
      | One -> 1                    
end

module TermSet = struct
  include TermSetBase
  let to_string ts = Printf.sprintf "{%s}" (String.concat ~sep:", " (List.map (elements ts) Term.to_string))
end

module Formula = struct
  type t =
    | Eq of Term.t * Term.t
    | Le of Term.t * Term.t
        
  let make_eq (t1:Term.t) (t2:Term.t) : t =
    Eq (t1,t2)

  let make_le (t1:Term.t) (t2:Term.t) : t =
    Le (t1,t2)

  let to_string (f:t) : string =
    match f with
      | Eq (s,t) ->
        Printf.sprintf "%s == %s"
          (Term.to_string s) (Term.to_string t)
      | Le (s,t) ->
        Printf.sprintf "%s <= %s"
          (Term.to_string s) (Term.to_string t)

  let compare (f1:t) (f2:t) : int =
    match f1,f2 with
      | Eq(s1,t1), Eq(s2,t2) ->
        let cmp = Term.compare s1 s2 in
        if cmp <> 0 then cmp
        else Term.compare t1 t2
      | Le(s1,t1), Le(s2,t2) ->
        let cmp = Term.compare s1 s2 in
        if cmp <> 0 then cmp
        else Term.compare t1 t2
      | Eq _, _ -> -1
      | _ -> 1

  let equal (f1:t) (f2:t) : bool =
    compare f1 f2 = 0

  let terms (f:t) =
    match f with
      | Eq (s,t) -> (s,t)
      | Le (s,t) -> (s,t)
end
