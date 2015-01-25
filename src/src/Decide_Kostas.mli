exception Empty

open Core.Std
open Decide_Util

type packet
type point
module PacketSet : sig
  include Set.S with type Elt.t = packet
end

 
module rec Term : sig
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
  val equal : t -> t -> bool
  val assg : Field.t -> Value.t -> t
  val test : Field.t -> Value.t -> t
  val dup : t
  val plus : TermSet.t -> t
  val times : t list -> t    
  val not : t -> t
  val star : t -> t
  val zero : t
  val one : t
  val compare_ab : t -> point -> bool
  val eval : t -> packet -> PacketSet.t
  val to_string : t -> string
  val values : t -> UnivMap.t
end and TermSet : sig
  include Set.S with type Elt.t = Term.t
end

module Formula : sig
  type t 
  val make_eq : Term.t -> Term.t -> t
  val make_le : Term.t -> Term.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val terms : t -> Term.t * Term.t
end

module type DerivTerm = sig
  
  type t with sexp

  module EMatrix : sig
    type t with sexp
    val fold : t -> init:'a -> f:('a -> point -> 'a) -> 'a
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
    val equivalent : (TermSet.t -> TermSet.t -> bool) -> t -> t -> bool
    val points : t -> EMatrix.t
  end
  
  val make_term : TermSet.t -> t
  val get_termset : t -> TermSet.t
  val get_e : t -> EMatrix.t
  val get_d : t -> DMatrix.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val compare : t -> t -> int
  val to_string : t -> string
end

(* module KostasDeriv : DerivTerm *)
module BDDDeriv : DerivTerm
