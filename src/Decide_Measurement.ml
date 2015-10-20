module Ast = Decide_Ast
module Util = Decide_Util

module Term = struct
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

type network = {
  ingress:  Ast.Term.t; (* in  *)
  outgress: Ast.Term.t; (* out *)
  p:        Ast.Term.t; (* p   *)
  t:        Ast.Term.t; (* t   *)
}

let compile {ingress; outgress; p; t} q =
  List.map Ast.Term.to_string [ingress; outgress; p; t]
  |> List.iter print_endline;
  failwith "TODO"
