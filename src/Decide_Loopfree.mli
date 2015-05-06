
module LoopChecker (D : Decide_Deriv.DerivTerm) : sig
  val loop_freedom : Decide_Ast.Term.t -> Decide_Ast.Term.t -> Decide_Ast.Term.t -> 'a -> bool
end
