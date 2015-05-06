open Core.Std
open Decide_Util
  
module FieldMap : sig
  include Map.S with type Key.t = Field.t
end
    
type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet with sexp, compare

val packet_to_string : packet -> string
val point_to_string : point -> string

module PacketSet : FiniteSet with type elt = packet
 
module rec Term : sig
  type t = term HashCons.hash_consed and
  term = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list
    | Intersection of TermSet.t
    | Not of t
    | Complement of t
    | Star of t
    | All
    | Zero
    | One with compare, sexp
  val equal : t -> t -> bool
  val assg : Field.t -> Value.t -> t
  val test : Field.t -> Value.t -> t
  val dup : t
  val plus : TermSet.t -> t
  val times : t list -> t
  val intersection : TermSet.t -> t
  val not : t -> t
  val all : t
  val complement : t -> t
  val star : t -> t
  val zero : t
  val one : t
  val compare_ab : t -> point -> bool
  val eval : t -> packet -> PacketSet.t
  val to_string : t -> string
  val values : t -> UnivMap.t
  val size : t -> int
end and TermSet : sig
  include Set.S with type Elt.t = Term.t
  val to_string : t -> string
end

module Path : sig
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

  val ( <+> ) : t -> t -> t
  val ( <*> ) : t -> t -> t
  val ( && ) : regex -> regex -> regex
  val ( || ) : regex -> regex -> regex
  val ( <.> ) : regex -> regex -> regex

  val regex_to_string : regex -> string
  val t_to_string : t -> string
  val translate : t -> Term.t
  val values : t -> UnivMap.t
end

module Formula : sig
  type t =
    | Neq of Term.t * Term.t
    | Eq of Term.t * Term.t
    | Le of Term.t * Term.t
    | Sat of Term.t * Path.t
    | Eval of Term.t
  val make_eq : Term.t -> Term.t -> t
  val make_neq : Term.t -> Term.t -> t
  val make_le : Term.t -> Term.t -> t
  val make_sat : Term.t -> Path.t -> t
  val make_eval : Term.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end
