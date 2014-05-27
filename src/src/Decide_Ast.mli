exception Empty

type cached_info = 
    { e_matrix : unit -> Decide_Base.Base.Set.t;
      one_dup_e_matrix : unit -> Decide_Base.Base.Set.t 
    }
      
module Term : sig
    
  type uid
    
  type t =
    | Assg of uid * Decide_Util.Field.t * Decide_Util.Value.t * cached_info option 
    | Test of uid * Decide_Util.Field.t * Decide_Util.Value.t * cached_info option
    | Dup of uid * cached_info option
    | Plus of uid * term_set * cached_info option 
    | Times of uid * t list * cached_info option 
    | Not of uid * t * cached_info option 
    | Star of uid * t * cached_info option 
    | Zero of uid * cached_info option
    | One of uid * cached_info option
  and term_set = (t) BatSet.PSet.t
      
    (* only for use in the parser *)
  val default_uid : uid
      
    val compare : t -> t -> int
      
    (* pretty printing *)
    val to_string : t -> string 
    val int_of_uid : uid -> int
      
  end 
    
  module TermSet : sig 
    type t = Term.term_set
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
  type term_set = Term.term_set
      
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
  val all_caches_empty : term -> bool
    
(* Pretty printing *)
  val term_to_string : term -> string
  val termset_to_string : (term) BatSet.PSet.t -> string
  val formula_to_string : formula -> string
    
(* more utils *)
  val memoize : (Term.t -> 'b) -> (Term.t -> 'b) 
  val memoize_on_arg2 : ('a -> Term.t -> 'c) -> ('a -> Term.t -> 'c)
 
