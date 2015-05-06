exception Quit

val debug_mode : bool
val profile_mode : bool
val failed_Count : int ref
val success_count : int ref

type stats = {
  compact_percent : int list ref
}

val stats : stats

val print_debugging_info : unit -> unit

module Field : sig
  type t with sexp
  val compare : t -> t -> int
  val hash : t -> int 
  val as_int : t -> int
  val equal : t -> t -> bool 
  val to_string : t -> string
  val of_string : string -> t
  
end
module FieldArray : sig
  type 'a t with sexp
  val make : 'a -> 'a t
  val init : (Field.t -> 'a) -> 'a t
  val set : 'a t -> Field.t -> 'a -> unit 
  val get : 'a t -> Field.t -> 'a
  val fold : ( Field.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val copy : 'a t-> 'a t
  val size : 'a t -> int
end 
module FieldSet : sig 
  include Set.S with type elt = Field.t
  val of_list : Field.t list -> t
end
  
module Value : sig
  type t with sexp
  val compare : t -> t -> int
  val hash : t -> int 
  val as_int : t -> int
  val equal : t -> t -> bool 
  val to_string : t -> string
  val of_string : string -> t
  val extra_val : t
end
module ValueSet : sig 
  include Set.S with type elt = Value.t
  val elt_of_sexp : Sexplib.Sexp.t -> elt
  val sexp_of_elt : elt -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val of_list : Value.t list -> t
end

module SetMapF :
  functor (K : Map.OrderedType) ->
  functor (V : Set.OrderedType) -> sig
    type t
    type elt = V.t
    module Values : Set.S with type elt = elt
    type eltSet = Values.t
    type key = K.t
    val empty : t
    val add : key -> elt -> t -> t
    val is_empty : t -> bool
    val union : t -> t -> t
    val keys : t -> key list
    val find_all : key -> t -> eltSet
    val filter : (key -> elt -> bool) -> t -> t
    val to_string : t -> (key -> string -> string, unit, string) format ->
      (elt list -> string list) -> string
  end

module UnivMap : sig 
  type t = SetMapF(Field)(Value).t
end

val all_fields : (unit -> FieldSet.t) ref 
val all_values : (unit -> (Field.t -> ValueSet.t)) ref
val set_univ : UnivMap.t list -> bool


module WorkList : functor (K:Set.OrderedType) -> 
sig
  type t 
  val add : K.t -> t -> t
  val singleton : K.t -> t
  val is_empty : t -> bool
  val hd : t -> K.t
  val tl : t -> t
  val all_seen_items : t -> K.t list
end

open Core.Std

module UnionFind : functor(Ord : Map.Key) -> 
sig
  type t with sexp
  module Class : sig
    type t with sexp
    val members : t -> Ord.t list
    val canonical_element : t -> Ord.t
  end
  val create : unit -> t
  val eq : t -> Ord.t -> Ord.t -> bool
  val find : t -> Ord.t -> Ord.t
  val union : t -> Ord.t -> Ord.t -> unit
  val validate : t -> unit
  val equivalence_classes : t -> Class.t list
end

val remove_duplicates : 'a list -> 'a list

val thunkify : (unit -> 'a) -> (unit -> 'a)

val string_fold : (char -> 'a -> 'a) -> string -> 'a -> 'a

module HashCons : sig

  type 'a hash_consed = private {
    node : 'a;
    tag : int
  } with sexp, compare

  module type HashedType = sig
    type t with sexp, compare
    val equal : t -> t -> bool
    val hash : t -> int
  end
  
  module Make (H : HashedType) : sig
    type t
    val create : int -> t
    val hashcons : t -> H.t -> H.t hash_consed
  end
end

module type Universe = sig
  type t with sexp, compare
  module S : Set.S with type Elt.t = t
  val universe : unit -> S.t
  val size : unit -> int
end

module type FiniteSet = sig
  type t with sexp
  type elt with sexp, compare

  val empty : t
  val all : t
  val singleton : elt -> t
  val complement : t -> t
  val length : t -> int
  val is_empty : t -> bool
  val mem : t -> elt -> bool
  val add : t -> elt -> t
  val remove : t -> elt -> t
  val subset : t -> t -> bool
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

module FiniteSet : functor (U : Universe) -> FiniteSet with type elt = U.t
