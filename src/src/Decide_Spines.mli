val lspines : unit Decide_Ast.term -> unit Decide_Ast.term_set
val rspines : unit Decide_Ast.term -> unit Decide_Ast.term_set
val lrspines : unit Decide_Ast.term -> unit Decide_Ast.term_set

module TermMap : sig 
  include Map.S 
end with type key = (unit Decide_Ast.term)

val allLRspines : unit Decide_Ast.term -> (unit Decide_Ast.term_set) TermMap.t


