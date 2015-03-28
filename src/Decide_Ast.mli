open Core.Std
open Decide_Util
  
module FieldMap : sig
  include Map.S with type Key.t = Field.t
end
    
type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet with sexp, compare

val packet_to_string : packet -> string
val point_to_string : point -> string

module PacketSet : sig
  type t with sexp
  type elt = packet

  val empty : t
  val all : t
  val singleton : elt -> t
  val complement : t -> t
  val length : t -> int
  val is_empty : t -> bool
  val mem : t -> elt -> bool
  val add : t -> elt -> t
  val remove : t -> elt -> t
  val union : t -> t -> t
  val union_list : t list -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val equal : t -> t -> bool
  val exists : t -> f:(elt -> bool) -> bool
  val for_all : t -> f:(elt -> bool) -> bool
  val count : t -> f:(elt -> bool) -> int
  val map : t -> f:(elt -> elt) -> t
  val filter_map : t -> f:(elt -> elt option) -> t
  val filter : t -> f:(elt -> bool) -> t
  val fold : t -> init:'a -> f:('a -> elt -> 'a) -> 'a
  val elements : t -> elt list
  val compare : t -> t -> int
end

 
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
    | Star of t
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

module Formula : sig
  type t 
  val make_eq : Term.t -> Term.t -> t
  val make_le : Term.t -> Term.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val terms : t -> Term.t * Term.t
end