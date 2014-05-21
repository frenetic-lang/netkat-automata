exception Empty

module Term : sig

  type uid

  type 'a t =
    | Assg of uid * Decide_Util.Field.t * Decide_Util.Value.t * 'a option 
    | Test of uid * Decide_Util.Field.t * Decide_Util.Value.t * 'a option
    | Dup of uid * 'a option
    | Plus of uid * 'a term_set * 'a option 
    | Times of uid * 'a t list * 'a option 
    | Not of uid * 'a t *'a option 
    | Star of uid * 'a t * 'a option 
    | Zero of uid * 'a option
    | One of uid * 'a option
  and 'a term_set = ('a t) BatSet.PSet.t

  (* only for use in the parser *)
  val default_uid : uid

  val compare : 'a t -> 'a t -> int

  (* pretty printing *)
  val to_string : 'a t -> string 
  val int_of_uid : uid -> int

end 

module TermSet : sig 
  type 'a t = 'a Term.term_set
  type 'a elt = 'a Term.t
  val singleton : 'a elt -> 'a t
  val empty : unit -> 'a t
  val add : 'a elt -> 'a t -> 'a t
  val map : ('a elt -> 'b elt) -> 'a t -> 'b t
  val fold : ('a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val union : 'a t -> 'a t -> 'a t
  val bind : 'a t -> ('a elt -> 'a t) -> 'a t
  val iter : ('a elt -> unit) -> 'a t -> unit
  val compare : 'a t -> 'a t -> int
  val elements : 'a t -> 'a elt list 
end


type 'a term = 'a Term.t
type 'a term_set = 'a Term.term_set

type 'a formula = Eq of 'a Term.t * 'a Term.t
	       | Le of 'a Term.t * 'a Term.t

(* smart constructors *)

val make_assg : Decide_Util.Field.t * Decide_Util.Value.t ->  'a Term.t
val make_test : Decide_Util.Field.t * Decide_Util.Value.t ->  'a Term.t
val make_dup :  'a Term.t
val make_plus : 'a Term.t list -> 'a Term.t
val make_times : 'a Term.t list -> 'a Term.t
val make_zero : 'a Term.t
val make_one : 'a Term.t

val parse_and_simplify : (string -> 'a formula) -> string -> 'a formula

module UnivMap : sig 
  type t = Decide_Util.SetMapF(Decide_Util.Field)(Decide_Util.Value).t
end

(* AST Utilities *)
val simplify : unit term -> unit term
val contains_dups : 'a term -> bool
val values_in_term : 'a term -> UnivMap.t 
val terms_in_formula : 'a formula -> 'a term * 'a term
val zero_dups : 'a term -> 'a term 
val one_dups : 'a term -> 'a term 

(* Pretty printing *)
val term_to_string : 'a term -> string
val termset_to_string : ('a term) BatSet.PSet.t -> string
val formula_to_string : 'a formula -> string

(* more utils *)
val memoize : ('a Term.t -> 'b) -> ('a Term.t -> 'b) 
val memoize_on_arg2 : ('a -> 'b Term.t -> 'c) -> ('a -> 'b Term.t -> 'c)
 
