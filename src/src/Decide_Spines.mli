module Spines : functor (U:Decide_Base.UnivDescr) -> sig
  module TermMap : sig 
    include Map.S 
  end with type key = (Decide_Ast_Cache.Ast(U).Term.t)

  val allLRspines : (Decide_Ast_Cache.Ast(U).Term.t) -> (Decide_Ast_Cache.Ast(U).cached_info Decide_Ast.term_set) TermMap.t
end


