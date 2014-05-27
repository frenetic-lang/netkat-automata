
module Bisimulation : functor (UDesc : Decide_Base.UnivDescr) -> sig 

  val check_equivalent : Decide_Ast.Ast(UDesc).term -> Decide_Ast.Ast(UDesc).term -> bool 
    
end


