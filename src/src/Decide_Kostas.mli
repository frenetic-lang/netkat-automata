open Core.Std
open Decide_Util

type packet
type point
module PacketSet : sig
  include Set.S with type Elt.t = packet
end

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
  val to_string : t -> string
end and TermSet : sig
  include Set.S with type Elt.t = Term.t
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

module KostasDeriv : DerivTerm
