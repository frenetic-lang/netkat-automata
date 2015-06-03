module SatChecker (D : Decide_Deriv.DerivTerm) : sig
  val check_sat : Decide_Ast.Term.t -> Decide_Ast.Path.t -> bool
end
