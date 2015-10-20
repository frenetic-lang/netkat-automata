module Ast = Decide_Ast
module Util = Decide_Util

(** The abstract syntax of our path query language. *)
module Predicate : sig
  type t =
    | One
    | Zero
    | Test of Util.Field.t * Util.Value.t
    | Or of t * t
    | And of t * t
    | Not of t

  val to_string: t -> string
end

module Query: sig
  type t =
    | Pred of Predicate.t * Predicate.t
    | Plus of t * t
    | Times of t * t
    | Star of t

  val to_string: t -> string
end

(** A network is described by [in; (p; t)*; p; out]. *)
type network = {
  ingress:  Ast.Term.t; (* in  *)
  outgress: Ast.Term.t; (* out *)
  p:        Ast.Term.t; (* p   *)
  t:        Ast.Term.t; (* t   *)
}

(** [compile n q] compiles a network [n] and query [q] into a NetKAT term [t]
 *  of which we take the E-matrix *)
val compile: network -> Query.t -> Ast.Term.t
