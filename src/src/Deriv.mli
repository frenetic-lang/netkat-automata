open Base

module Deriv : functor (U:UnivDescr) -> 
sig
  type deriv_term 
  val compare : deriv_term -> deriv_term -> int
  val run_e : deriv_term -> Univ(U).Base.Set.t
  val run_d : deriv_term -> ((Univ(U).Base.point -> deriv_term) * Univ(U).Base.Set.t)
  val make_term : Ast.Term.term -> deriv_term
  val to_term : deriv_term -> Ast.term
end
