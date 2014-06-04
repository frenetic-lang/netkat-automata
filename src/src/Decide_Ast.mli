exception Empty

type cached_info = 
    { e_matrix : unit -> Decide_Base.Base.Set.t;
      one_dup_e_matrix : unit -> Decide_Base.Base.Set.t
    }
      
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
		   
val make_assg : Decide_Util.Field.t * Decide_Util.Value.t ->  Term.t
val make_test : Decide_Util.Field.t * Decide_Util.Value.t ->  Term.t
val make_dup :  Term.t
val make_plus : Term.t list -> Term.t
val make_times : Term.t list -> Term.t
val make_zero : Term.t
val make_one : Term.t
val make_not : Term.t -> Term.t
val make_star : Term.t -> Term.t
  
(* AST Utilities *)
val get_cache : term -> cached_info
val values_in_term : term -> UnivMap.t 
val allLRspines : term -> TermPairSet.t  TermMap.t

  
(* more utils *)
val memoize : (Term.t -> 'b) -> (Term.t -> 'b) 
val memoize_on_arg2 : ('a -> Term.t -> 'c) -> ('a -> Term.t -> 'c)

