open Decide_Base

module DerivTerm : sig
  type t 
  val compare : t -> t -> int
  val make_term : Decide_Ast.Term.t -> t
  val to_string : t -> string
end
val run_e : DerivTerm.t -> Base.Set.t
val run_d : DerivTerm.t -> 
  ((Base.point -> DerivTerm.t) * Base.Set.t)

