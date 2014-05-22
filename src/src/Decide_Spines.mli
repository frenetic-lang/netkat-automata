module Spines : functor (U:Decide_Base.UnivDescr) -> sig
  module TermMap : sig 
    include Map.S 
  end with type key = unit Decide_Ast.term

  module TermPairSet : sig
    include Set.S
    val map : (elt -> elt) -> t -> t
  end with type elt = unit Decide_Ast.term * unit Decide_Ast.term

  val allLRspines : unit Decide_Ast.term -> TermPairSet.t  TermMap.t
end


