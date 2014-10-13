open Decide_Base

module DerivTerm : sig
  type t 
  val compare : t -> t -> int
  val make_term : Decide_Ast.Term.t -> t
  (* val to_term : t -> Decide_Ast.Term.t *)
  val to_string : t -> string
  val run_e : t -> Base.Set.t
  val run_d : t -> ((Base.point -> t) * Base.Set.t)
  val sexp_of_t : t -> Sexplib.Sexp.t
end


