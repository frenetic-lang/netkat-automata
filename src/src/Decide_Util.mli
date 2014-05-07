exception Quit

module IntSetMap : sig 
    type t
    type elt = int
    module Values : Set.S with type elt = elt
    type eltSet = Values.t
    type key = int
    val empty : t
    val add : key -> elt -> t -> t
    val add_all : key -> eltSet -> t -> t
    val remove : key -> elt -> t -> t
    val remove_all : key -> t -> t
    val find_all : key -> t -> eltSet
    val contains_key : key -> t -> bool
    val contains_value : key -> elt -> t -> bool
    val size : key -> t -> int
    val keys : t -> key list
    val bindings : t -> (key * elt list) list
    val iter : (key -> elt -> unit) -> t -> unit
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val fold : (key -> elt -> 'b -> 'b) -> t -> 'b -> 'b
    val fold_key : (elt -> 'b -> 'b) -> key -> t -> 'b -> 'b
    val filter : (key -> elt -> bool) -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val consis : key -> elt -> t -> bool
    val single_mapping : key -> t -> bool
    val for_all : (key -> elt -> bool) -> t -> bool
    val is_empty : t -> bool
    val val_inter : eltSet -> eltSet -> eltSet
    val val_equal : eltSet -> eltSet -> bool
    val val_is_empty : eltSet -> bool
    val val_empty : eltSet
    val val_mem : elt -> eltSet -> bool
    val val_size : eltSet -> int
    val val_singleton : elt -> eltSet
    val maps_to_empty : key -> t -> bool
    val to_string : t -> (key -> string -> string, unit, string) format ->
      (elt list -> string list) -> string
end

module WorkList : functor (K:Set.OrderedType) -> 
sig
  type t 
  val add : K.t -> t -> t
  val singleton : K.t -> t
  val is_empty : t -> bool
  val hd : t -> K.t
  val tl : t -> t
end

type 'a union_find_ds
val init_union_find : unit -> 
(('a union_find_ds ref -> 'a union_find_ds ref -> bool)* 
	('a -> 'a union_find_ds ref) * 
	('a union_find_ds ref -> 'a union_find_ds ref -> 
	 'a union_find_ds ref))

val remove_duplicates : 'a list -> 'a list
val memoize_on_arg2 : ('a -> 'b -> 'c) -> ('a -> 'b -> 'c)
val memoize : ('a -> 'c) -> ('a  -> 'c)
