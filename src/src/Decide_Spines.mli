module Spines : functor (U:Decide_Base.UnivDescr) -> sig
  module TermMap : sig 
    include Map.S 
  end with type key = Decide_Ast.Ast(U).term

  module TermPairSet : sig
    include Set.S
    val map : (elt -> elt) -> t -> t
  end with type elt = Decide_Ast.Ast(U).term * Decide_Ast.Ast(U).term

  val allLRspines : Decide_Ast.Ast(U).term -> TermPairSet.t  TermMap.t
end


