module type UnivDescr = sig
  type field = Decide_Ast.Term.Field.t
  type value = Decide_Ast.Term.Value.t
  module FieldSet : Set.S with type elt = field
  module ValueSet : Set.S with type elt = value
  val all_fields : FieldSet.t
  val all_values : field -> ValueSet.t
end

module Univ : functor (U:UnivDescr) ->
sig
  module Base : sig
    type t
    val compare : t -> t -> int
    type point
    val compare_point : point -> point -> int
    val point_to_string : point -> string
    type complete_test 
    val point_rhs : point -> complete_test
    val point_lhs : point -> complete_test
    val compare_complete_test : complete_test -> complete_test -> int
    val complete_test_to_string : complete_test -> string
    val completetest_to_term_test : complete_test -> Decide_Ast.InitialTerm.t

    val project_lhs : t -> t
    module Set : sig 
      include Set.S with type elt = t
      val shallow_equal : t -> t -> bool
      val to_string : t -> string
      val fold_points : (point -> 'a -> 'a) -> t -> 'a -> 'a
      val contains_point : t -> point -> bool
      val filter_alpha : t -> complete_test -> t
      val of_term : Decide_Ast.term -> t
      val mult : t -> t -> t

    (* debugging *)
      val print_debugging_info : unit -> unit
    end
  end
end
