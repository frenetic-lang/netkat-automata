open Decide_Base

module Deriv : functor (U:UnivDescr) -> 
sig
  module DerivTerm : sig
    type t 
    val compare : t -> t -> int
    val to_term : t -> Decide_Ast.term
    val make_term : Decide_Ast.Term.term -> t
  end
  val run_e : DerivTerm.t -> Univ(U).Base.Set.t
  val run_d : DerivTerm.t -> ((Univ(U).Base.point -> DerivTerm.t) * Univ(U).Base.Set.t)

end
