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

module DerivTerm : sig
  type t with sexp
  type e with sexp
  type d with sexp
  val make_term : Term.t -> t
  (* val get_term : t -> Term.t *)
  (* val to_term : t -> Decide_Ast.Term.t *)
  val get_e : t -> e
  val get_d : t -> d
  val sexp_of_t : t -> Sexplib.Sexp.t
  val run_e : e -> point -> bool
  val run_d : d -> point -> TermSet.t
end
