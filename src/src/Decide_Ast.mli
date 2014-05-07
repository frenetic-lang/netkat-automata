exception Empty

type id = string

module rec Term : sig
  type term =
  | Assg of id * string
  | Test of id * string
  | Dup
  | Plus of TermSet.t
  | Times of term list
  | Not of term
  | Star of term
  | Zero
  | One
end and TermSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
  val from_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val return : elt -> t
end with type elt = Term.term 

type term = Term.term

type formula = 
  | Eq of term * term 
  | Le of term * term

(* AST Utilities *)
val contains_dups : term -> bool
val values_in_term : term -> Decide_Util.StringSetMap.t
val values_in_formula : formula -> Decide_Util.StringSetMap.t
val terms_in_formula : formula -> term * term
val simplify : term -> term
val simplify_formula : formula -> formula
val deMorgan : term -> term 
val zero_dups : term -> term 
val one_dups : term -> term 

(* Ye olde matrix stuff *)
val mul_terms : term -> term -> term
val add_terms : term -> term -> term

(* Pretty printing *)
val term_to_string : term -> string
val termset_to_string : TermSet.t -> string
val formula_to_string : formula -> string
