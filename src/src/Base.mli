module type UnivDescr = sig
  val univ : Util.StringSetMap.t
end

module Univ : functor (U:UnivDescr) -> 
sig 
  module Element : sig 
    type t 
    module Set : Set.S with type elt = t
  end 

  module Base : sig 
    type t
    val to_test : t -> Element.t
    val to_assign : t -> Element.t
    val to_string : t -> string
    module Set : sig
      include Set.S with type elt = Base.t
      val fold : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
      val to_string : t -> string
      val to_matrix_string : t -> string
    end
  end 

  val calculate_E : Ast.term -> Base.Set.t
end
