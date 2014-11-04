module D = Decide_Deriv.DerivTerm
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
    
module rec Term : sig
  type t = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One with compare, sexp
  val compare_ab : t -> point -> bool
  val eval : t -> packet -> PacketSet.t
end = struct 
  type t = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One with compare, sexp

  let rec eval (t : Term.t) (pkt : packet) = match t with
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
      
end and TermSet : sig
  include Set.S with type Elt.t = Term.t
end = Set.Make (struct
      type t = Term.t with compare, sexp
    end)
  
module rec DerivTerm : sig
  type t with sexp
  type e with sexp
  type d with sexp
  (* val make_term : Term.t -> t *)
  (* val get_term : t -> Term.t *)
  (* val to_term : t -> Decide_Ast.Term.t *)
  val get_e : t -> e
  val get_d : t -> d
  val sexp_of_t : t -> Sexplib.Sexp.t
  val run_e : e -> point -> bool
  val e_matrix_of_term : Term.t -> e
  val run_d : d -> point -> TermSet.t
  val d_matrix_of_term : Term.t -> d
end = struct
  type e = Term.t with sexp  
  let run_e = Term.compare_ab
  let rec e_matrix_of_term t =
    let open Term in
    match t with
    | Plus ts -> Plus (TermSet.map ts e_matrix_of_term)
    | Dup -> Zero
    | Times ts -> Times (List.map ts e_matrix_of_term)
    | Star t -> Star (e_matrix_of_term t)
    | _ -> t

  type compact_derivative = {
    left_hand : Term.t;
    right_hand : Term.t;
  } with compare, sexp

  module CompactDerivSet = Set.Make (struct
      type t = compact_derivative with compare, sexp
    end)

  type d = CompactDerivSet.t with sexp

  type t = { desc : Term.t;
             e_matrix : e;
             d_matrix : d
           } with sexp
  let get_e t = t.e_matrix
  let get_d t = t.d_matrix

  let run_d t point =
    CompactDerivSet.fold t ~init:TermSet.empty
      ~f:(fun acc deriv -> if Term.compare_ab deriv.left_hand point
           then TermSet.union (TermSet.singleton deriv.right_hand) acc
           else acc)
  let term_append t e = Term.Times [t; e]

  let right_app d e = { d with right_hand = term_append d.right_hand e }
  let left_app e d = { d with left_hand = term_append d.left_hand e }

  let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e)
  let d_left_app e ds = CompactDerivSet.map ds (left_app e)


  let rec d_matrix_of_term t =
    let open Term in
    match t with
    | Dup -> CompactDerivSet.singleton ({ left_hand = One; right_hand = One })
    | Plus ts -> TermSet.fold ts ~f:(fun acc t -> CompactDerivSet.union (d_matrix_of_term t) acc) ~init:CompactDerivSet.empty
    | Times (t::ts) -> CompactDerivSet.union (d_right_app (d_matrix_of_term t) (Times ts))
                         (d_left_app (e_matrix_of_term t) (d_matrix_of_term (Times ts)))
    | Star t -> d_left_app (Star t) (d_right_app (d_matrix_of_term t) (Star t))
    | _ -> CompactDerivSet.empty

end

