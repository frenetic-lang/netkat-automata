exception Empty

module rec Term : sig
  type uid
  (* only for use in the parser *)
  val default_uid : uid
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

type term = Term.t

(* smart constructors *)
val make_assg : Decide_Util.Field.t * Decide_Util.Value.t -> Term.t
val make_test : Decide_Util.Field.t * Decide_Util.Value.t -> Term.t
val make_dup :  Term.t 
val make_plus : Term.t list -> Term.t 
val make_times : Term.t list -> Term.t
val make_not : Term.t -> Term.t
val make_star : Term.t -> Term.t 
val make_zero :  Term.t 
val make_one :  Term.t
val assign_ids : Term.t -> Term.t

module UnivMap : sig 
  type t = Decide_Util.SetMapF(Decide_Util.Field)(Decide_Util.Value).t
end

type formula = 
  | Eq of Term.t * Term.t 
  | Le of Term.t * Term.t

(* AST Utilities *)
val parse_and_simplify : (string -> term) -> string -> term
val contains_dups : term -> bool
val values_in_term : term -> UnivMap.t 
val terms_in_formula : formula -> term * term
val zero_dups : term -> term 
val one_dups : term -> term 
val one : term 
val zero : term

(* Pretty printing *)
val term_to_string : term -> string
val termset_to_string : TermSet.t -> string
val formula_to_string : formula -> string

(* more utils *)
val memoize : (Term.t -> 'a) -> (Term.t -> 'a) 
val memoize_on_arg2 : ('a -> Term.t -> 'c) -> ('a -> Term.t -> 'c)
