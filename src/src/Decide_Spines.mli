module TermMap : sig 
  include Map.S 
end with type key = Decide_Ast.term
  
module TermPairSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
end with type elt = Decide_Ast.term * Decide_Ast.term
  
val allLRspines : Decide_Ast.term -> TermPairSet.t  TermMap.t
    


