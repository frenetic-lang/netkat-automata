
module Ast : functor (U : Decide_Base.UnivDescr) -> sig 

  type cached_info = { e_matrix : Decide_Base.Univ(U).Base.Set.t;
		       one_dup_e_matrix : Decide_Base.Univ(U).Base.Set.t 
		     }

  module Term : sig 
    type t = cached_info Decide_Ast.term
    val compare : t -> t -> int 
  end

  val get_cache : cached_info Decide_Ast.term -> cached_info
    
  val of_term : 'a Decide_Ast.term -> Term.t
  val refresh_cache : Term.t -> Term.t

  val all_caches_empty : 'a Decide_Ast.term -> bool 

  val zero : Term.t
  val one : Term.t
  val plus : cached_info Decide_Ast.TermSet.t -> Term.t
  val times : Term.t list -> Term.t

end
