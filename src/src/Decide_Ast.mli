exception Empty
      
module Term : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string 
  val hash : t -> int
  end 

module TermSet : sig 
  include Set.S
  val map : (elt -> elt) -> t -> t
  val bind : t -> (elt -> t) -> t
  val to_string : t -> string
end with type elt = Term.t

module TermMap : sig 
  include Map.S 
end with type key = Term.t
  
module TermPairSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
end with type elt = Term.t * Term.t
  
module UnivMap : sig 
  type t = Decide_Util.SetMapF(Decide_Util.Field)(Decide_Util.Value).t
end

type term = Term.t
    
(* smart constructors *)
		   
val make_assg : Decide_Util.Field.t * Decide_Util.Value.t ->  term
val make_test : Decide_Util.Field.t * Decide_Util.Value.t ->  term
val make_dup :  term
val make_plus : term list -> term
val make_times : term list -> term
val make_zero : term
val make_one : term
val make_not : term -> term
val make_star : term -> term
  
(* AST Utilities *)
val e_matrix : term -> Decide_Base.Base.Set.t
val one_dup_e_matrix : term -> Decide_Base.Base.Set.t

val values_in_term : term -> UnivMap.t 
val lrspines : term -> TermPairSet.t

(* more utils *)
val memoize : (term -> 'b) -> (term -> 'b) 
val memoize_on_arg2 : ('a -> term -> 'c) -> ('a -> term -> 'c)

