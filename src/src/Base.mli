module type UnivDescr = sig
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
  val field_of_id : Ast.id -> field
  val value_of_id : Ast.id -> value
  val value_of_string : string -> value
end

module Univ : functor (U:UnivDescr) ->
sig
  module Base : sig
    type t
    type point
    val project_lhs : t -> t
    module Set : sig 
      include Set.S with type elt = t
      val to_string : t -> string
      val fold_points : (point -> 'a -> 'a) -> t -> 'a -> 'a
      val contains_point : t -> point -> bool
      val of_term : Ast.term -> t
      val mult : t -> t -> t
    end
    val test_of_point_left : point -> Ast.term
    val test_of_point_right : point -> Ast.term
  end
end
