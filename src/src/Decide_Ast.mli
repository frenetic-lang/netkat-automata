exception Empty

module UnivMap : sig 
  type t = Decide_Util.SetMapF(Decide_Util.Field)(Decide_Util.Value).t
end
      
module rec Term : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string 
  val hash : t -> int

  (* smart constructors *)
    
  val make_assg : Decide_Util.Field.t * Decide_Util.Value.t ->  t
  val make_test : Decide_Util.Field.t * Decide_Util.Value.t ->  t
  val make_dup :  unit -> t
  val make_plus : TermSet.t -> t
  val make_times : t list -> t
  val make_zero : unit -> t
  val make_one : unit -> t
  val make_not : t -> t
  val make_star : t -> t

  val of_complete_test : Decide_Base.Base.complete_test -> t

  val e_matrix : t -> Decide_Base.Base.Set.t
  val one_dup_e_matrix : t -> Decide_Base.Base.Set.t
  val lrspines : t -> TermPairSet.t


  val fields : t -> Decide_Util.FieldSet.t
  val values : t -> UnivMap.t

end and TermSet : sig 
  include Set.S with type elt = Term.t
  val map : (elt -> elt) -> t -> t
  val bind : t -> (elt -> t) -> t
  val of_list : elt list -> t
  val to_string : t -> string
end and TermPairSet : sig 
  include Set.S with type elt = Term.t * Term.t 
  val map : (elt -> elt) -> t -> t
end

module TermMap : sig 
  include Map.S 
end with type key = Term.t
  

module Formula : sig
  type t 
  val make_eq : Term.t -> Term.t -> t
  val make_le : Term.t -> Term.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val terms : t -> Term.t * Term.t
end
  
  
(* more utils *)
val memoize : (Term.t -> 'b) -> (Term.t -> 'b) 


