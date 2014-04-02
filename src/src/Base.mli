module type UnivDescr = sig
  val univ : Util.StringSetMap.t
end

module Univ : functor (U:UnivDescr) -> 
sig 
  module Index : sig 
    type t 
    val to_test : t -> Ast.term
    val to_string : t -> string
  end 

  module IndexPairSet : sig 
    include Set.S with type elt = (Index.t * Index.t)
  end 

  module BaseElt : sig 
    type t 
    val alpha_of_index : Index.t -> t
    val beta_of_index : t -> Index.t -> t 
  end 

  module Base : sig 
    type t = BaseElt.t * BaseElt.t
    val to_string : t -> string
  end 

  module BaseSet : sig 
    include Set.S with type elt = Base.t
    (* our stuff *)
    val contains : BaseElt.t -> BaseElt.t -> t -> bool
    val non_empty : t -> bool (* TODO(jnf): this is some kind of Milanoesque semantic non-emptiness *)
    val to_IndexPairSet : t -> IndexPairSet.t
    val to_string : t -> string
    val to_matrix_string : t -> string
  end 

  val calculate_E : Ast.term -> BaseSet.t
end
