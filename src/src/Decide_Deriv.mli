open Decide_Base

module DerivTerm : sig
  type t 
  val compare : t -> t -> int
  val make_term : Decide_Ast.Term.t -> t
  val to_string : t -> string
  val run_e : t -> Base.Set.t
  val run_d : t -> ((Base.point -> t) * Base.Set.t)
end


