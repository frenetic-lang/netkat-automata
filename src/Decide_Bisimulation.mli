
module EquivChecker (D : Decide_Deriv.DerivTerm) : sig
  val check_equivalent : Decide_Ast.Term.t -> Decide_Ast.Term.t -> bool 
  val check_certificate : string -> unit
  val check_eval : Decide_Ast.Term.t -> unit
end
