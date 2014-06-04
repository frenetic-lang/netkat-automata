exception Empty

type cached_info = 
    { e_matrix : unit -> Decide_Base.Base.Set.t;
      one_dup_e_matrix : unit -> Decide_Base.Base.Set.t 
    }
      
module Term : sig
    
  type t
  val compare : t -> t -> int
    (* pretty printing *)
  val to_string : t -> string 
  val to_string_sexpr : t -> string
  end 

module TermSet : sig 
  type t
  type elt = Term.t
  val singleton : elt -> t
  val empty : t
  val add : elt -> t -> t
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val union : t -> t -> t
  val bind : t -> (elt -> t) -> t
  val iter : (elt -> unit) -> t -> unit
  val compare : t -> t -> int
  val elements : t -> elt list 
end
  
type term = Term.t
    
type formula = Eq of Term.t * Term.t
	       | Le of Term.t * Term.t
		   
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
  
val convert_and_simplify : ('s -> formula) -> 's -> formula
  
module UnivMap : sig 
  type t = Decide_Util.SetMapF(Decide_Util.Field)(Decide_Util.Value).t
end
  
(* AST Utilities *)
val get_cache : term -> cached_info
val simplify : term -> term
val contains_dups : term -> bool
val values_in_term : term -> UnivMap.t 
val terms_in_formula : formula -> term * term
val zero_dups : term -> term 
val all_ids_assigned : term -> bool
val no_caches_empty : term -> bool
  
(* Pretty printing *)
val term_to_string : term -> string
val termset_to_string : (term) BatSet.PSet.t -> string
val formula_to_string : formula -> string
  
(* more utils *)
val memoize : (Term.t -> 'b) -> (Term.t -> 'b) 
val memoize_on_arg2 : ('a -> Term.t -> 'c) -> ('a -> Term.t -> 'c)

module Decide_Spines : sig 
  module TermMap : sig 
    include Map.S 
  end with type key = term
    
  module TermPairSet : sig
    include Set.S
    val map : (elt -> elt) -> t -> t
  end with type elt = term * term
    
  val allLRspines : term -> TermPairSet.t  TermMap.t
    
end
