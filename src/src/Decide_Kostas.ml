module S = Sexplib.Sexp
module Base = Decide_Base.Base
                
open Core.Std
open Sexplib.Conv
open Decide_Util

module FieldMap = Map.Make(Field)

type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet

module PacketSet = Set.Make (struct
    type t = packet with sexp, compare
  end)

  
module rec TermBase : sig
  type t = term HashCons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
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
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One with compare, sexp

end and TermSet : sig
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
    | Plus ts -> TermSet.fold ts ~f:(fun acc t -> PacketSet.union (eval t pkt) acc) ~init:PacketSet.empty
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
        assoc_to_string " + " "0" 
          (List.map ~f:protect (TermSet.elements ts))
      | Times (ts) -> 
        assoc_to_string ";" "1" (List.map ~f:protect ts)
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
  let assg f v = H.hashcons hashtbl (Assg (f,v))
  let test f v = H.hashcons hashtbl (Test (f,v))
  let dup = H.hashcons hashtbl Dup
  let plus ts = H.hashcons hashtbl (Plus ts)
  let times ts = H.hashcons hashtbl (Times ts)
  let not t = H.hashcons hashtbl (Not t)
  let star t = H.hashcons hashtbl (Star t)
  let zero = H.hashcons hashtbl Zero
  let one = H.hashcons hashtbl One
end

module type DerivTerm = sig
    module EMatrix : sig
    type t with sexp
    val run : t -> point -> bool
    val compare : t -> t -> int
    val empty : t
    val intersection_empty : t -> t -> bool
    val union : t -> t -> t
  end

  module DMatrix : sig
    type t with sexp
    val run : t -> point -> TermSet.t
    val compare : t -> t -> int
    val equivalent : (Term.t -> Term.t -> bool) -> t -> t -> bool
  end
  
  type t with sexp  
  val make_term : Term.t -> t
  (* val get_term : t -> Term.t *)
  (* val to_term : t -> Decide_Ast.Term.t *)
  val get_e : t -> EMatrix.t
  val get_d : t -> DMatrix.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val compare : t -> t -> int
end

module rec KostasDeriv : DerivTerm = struct
  
  module EMatrix = struct
    type t = Term.t with sexp  
    let run = Term.compare_ab
    open HashCons
                
    let rec matrix_of_term t =
      let open Term in
      let open TermBase in
      match t.node with
      | Plus ts -> plus (TermSet.map ts matrix_of_term)
      | Dup -> zero
      | Times ts -> times (List.map ts matrix_of_term)
      | Star t -> star (matrix_of_term t)
      | _ -> t
        
    let compare _ _ = failwith "NYI: Decide_Kostas.DerivTerm.EMatrix.compare"
    let intersection_empty e e' = failwith "NYI: Decide_Kostas.EMatrix.intersection"
    let empty = Term.zero
    let union e1 e2 = Term.plus (TermSet.of_list [e1;e2])
  end

  module DMatrix = struct

    open HashCons

    type compact_derivative = {
      left_hand : EMatrix.t;
      right_hand : Term.t;
    } with compare, sexp

    module CompactDerivSet = Set.Make (struct
        type t = compact_derivative with compare, sexp
      end)

    type t = CompactDerivSet.t with sexp

    let compare _ _ = failwith "NYI: Decide_Kostas.DerivTerm.EMatrix.compare"
        
    let run t point =
      CompactDerivSet.fold t ~init:TermSet.empty
        ~f:(fun acc deriv -> if Term.compare_ab deriv.left_hand point
             then TermSet.union (TermSet.singleton deriv.right_hand) acc
             else acc)

    let term_append t e = Term.times [t; e]
    let left_app e d = { d with left_hand = term_append d.left_hand e }
    let right_app d e = { d with right_hand = term_append d.right_hand e }

    let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e)
    let d_left_app e ds = CompactDerivSet.map ds (left_app e)


    let rec matrix_of_term t =
      let open Term in
      match t.node with
      | Dup -> CompactDerivSet.singleton ({ left_hand = one; right_hand = one })
      | Plus ts -> TermSet.fold ts ~f:(fun acc t -> CompactDerivSet.union (matrix_of_term t) acc) ~init:CompactDerivSet.empty
      | Times (t::ts) -> CompactDerivSet.union (d_right_app (matrix_of_term t) (times ts))
                           (d_left_app (EMatrix.matrix_of_term t) (matrix_of_term (times ts)))
      | Star t -> d_left_app (star t) (d_right_app (matrix_of_term t) (star t))
      | _ -> CompactDerivSet.empty

    (* 
       a) for each (b, e) \in D(elm1), (b',e') \in D(elm2), 
          if b /\ b' != 0, then e bisim e'
       b) \/ b == \/ b'
    *)
    let equivalent bisim d1 d2 =
      CompactDerivSet.for_all d1 ~f:(fun elm1 -> CompactDerivSet.for_all d2 ~f:(fun elm2 ->
          EMatrix.intersection_empty elm1.left_hand elm2.left_hand
          || bisim elm1.right_hand elm2.right_hand))
      && EMatrix.compare (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc))
        (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc)) = 0
  end

  type t = { desc : Term.t;
             e_matrix : EMatrix.t;
             d_matrix : DMatrix.t
           } with sexp
    
  let get_e t = t.e_matrix
  let get_d t = t.d_matrix
  let get_term t = t.desc

      
  let make_term term =
    { desc = term;
      e_matrix = EMatrix.matrix_of_term term;
      d_matrix = DMatrix.matrix_of_term term
    }

  let compare t t' = failwith "NYI: Decide_Kostas.KostasDeriv.compare"
end
