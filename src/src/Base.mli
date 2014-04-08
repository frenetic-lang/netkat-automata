module type UnivDescr = sig 
  type t 
  type field 
  type value 
  module FieldSet : Set.S with type elt = field
  module ValueSet : Set.S with type elt = value
  val field_compare : field -> field -> int
  val value_compare : value -> value -> int
  val all_fields : FieldSet.t
  val all_values : field -> ValueSet.t
  val field_to_string : field -> string
  val value_to_string : value -> string
end

module Univ : functor (U:UnivDescr) -> 
sig 
  module Base : sig 
    type t
    type point
    module Set : Set.S with type elt = t
    val fold_points : (point -> 'a -> 'a) -> t -> 'a -> 'a
    val contains_point : BaseSet.t -> point -> bool
    val calculate_E : Ast.term -> Base.Set.t
  end
end
