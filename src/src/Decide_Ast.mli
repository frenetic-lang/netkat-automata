exception Empty


module rec Term : sig
  module Field : sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
    val of_string : string -> t
  end
  module Value : sig
    type t 
    val compare : t -> t -> int
    val to_string : t -> string
    val of_string : string -> t
    val extra_val : t
  end
  type uid
  type term =
    | Assg of uid * Field.t * Value.t
    | Test of uid * Field.t * Value.t
    | Dup of uid 
    | Plus of uid * TermSet.t
    | Times of uid * term list
    | Not of uid * term
    | Star of uid * term
    | Zero of uid
    | One of uid
  val compare : term -> term -> int
  val to_string : term -> string 

end and TermSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
  val from_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val return : elt -> t
end with type elt = Term.term 

module rec InitialTerm : sig
  type term =
    | Assg of Term.Field.t * Term.Value.t
    | Test of Term.Field.t * Term.Value.t
    | Dup 
    | Plus of  InitialTermSet.t
    | Times of term list
    | Not of term
    | Star of term
    | Zero 
    | One 
  val to_term : term -> Term.term
end and InitialTermSet : sig
  include Set.S
  val from_list : elt list -> t
end with type elt = InitialTerm.term 


type term = Term.term

module UnivMap : sig 
  type t = Decide_Util.SetMapF(Term.Field)(Term.Value).t
end


type formula = 
  | Eq of InitialTerm.term * InitialTerm.term 
  | Le of InitialTerm.term * InitialTerm.term

(* AST Utilities *)
val contains_dups : term -> bool
val values_in_term : term -> UnivMap.t 
val terms_in_formula : formula -> term * term
val simplify : term -> term
val simplify_formula : formula -> formula
val deMorgan : term -> term 
val zero_dups : term -> term 
val one_dups : term -> term 
val one : term 
val zero : term

(* Ye olde matrix stuff *)
val mul_terms : term -> term -> term
val add_terms : term -> term -> term

(* Pretty printing *)
val term_to_string : term -> string
val termset_to_string : TermSet.t -> string
val formula_to_string : formula -> string

module Decide_Spines : sig 
    val lspines : term -> TermSet.t
    val rspines : term -> TermSet.t
    val lrspines : term -> TermSet.t 
    val allLRspines : term -> (term, TermSet.t) Hashtbl.t
end
