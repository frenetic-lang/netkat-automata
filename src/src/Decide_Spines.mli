module Spines : functor (U:Decide_Base.UnivDescr) -> sig
  module TermMap : sig 
    include Map.S 
  end with type key = unit Decide_Ast.term

  val allLRspines : unit Decide_Ast.term -> unit Decide_Ast.term_set TermMap.t
end


