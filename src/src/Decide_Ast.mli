exception Empty


module rec Term : sig
 
  type uid
  val int_of_uid : uid -> int
  val largest_uid : unit -> uid
  type t =
    | Assg of uid * Decide_Util.Field.t * Decide_Util.Value.t
    | Test of uid * Decide_Util.Field.t * Decide_Util.Value.t
    | Dup of uid 
    | Plus of uid * TermSet.t
    | Times of uid * t list
    | Not of uid * t
    | Star of uid * t
    | Zero of uid
    | One of uid
  val compare : t -> t -> int
  val to_string : t -> string 

end and TermSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
  val from_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val return : elt -> t
end with type elt = Term.t 

module rec InitialTerm : sig
  type t =
    | Assg of Decide_Util.Field.t * Decide_Util.Value.t
    | Test of Decide_Util.Field.t * Decide_Util.Value.t
    | Dup 
    | Plus of  InitialTermSet.t
    | Times of t list
    | Not of t
    | Star of t
    | Zero 
    | One 
  val to_term : t -> Term.t
  val of_term : Term.t -> t
end and InitialTermSet : sig
  include Set.S
  val map : (elt -> elt) -> t -> t
  val from_list : elt list -> t
  val bind : t -> (elt -> t) -> t
  val return : elt -> t

end with type elt = InitialTerm.t 


type term = Term.t

module UnivMap : sig 
  type t = Decide_Util.SetMapF(Decide_Util.Field)(Decide_Util.Value).t
end


type formula = 
  | Eq of InitialTerm.t * InitialTerm.t 
  | Le of InitialTerm.t * InitialTerm.t

(* AST Utilities *)
val contains_dups : term -> bool
val values_in_term : term -> UnivMap.t 
val terms_in_formula : formula -> term * term
val simplify : term -> term
val simplify_tt : InitialTerm.t -> InitialTerm.t
val simplify_formula : formula -> formula
val deMorgan : term -> term 
val zero_dups : term -> term 
val one_dups : term -> term 
val one : term 
val zero : term


(* Pretty printing *)
val term_to_string : term -> string
val termset_to_string : TermSet.t -> string
val formula_to_string : formula -> string
val serialize_formula : formula -> string -> unit

val memoize : (Term.t -> 'a) -> (Term.t -> 'a) 
val memoize_on_arg2 : ('a -> Term.t -> 'c) -> ('a -> Term.t -> 'c)
