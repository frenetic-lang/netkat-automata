module Ast = Decide_Ast
module Util = Decide_Util

(** The abstract syntax of our path query language. *)
module Term : sig
  type predicate =
    | One
    | Zero
    | Test of Util.Field.t * Util.Value.t
    | Or of predicate * predicate
    | And of predicate * predicate
    | Not of predicate

  type t =
    | Pred of predicate
    | Plus of t * t
    | Times of t * t
    | Star of t
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
val compile: network -> Term.t -> Ast.Term.t
