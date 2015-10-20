module Ast = Decide_Ast
module Util = Decide_Util

module Predicate = struct
  type t =
    | One
    | Zero
    | Test of Util.Field.t * Util.Value.t
    | Or of t * t
    | And of t * t
    | Not of t

  let rec to_string pred =
    match pred with
    | One         -> "1"
    | Zero        -> "0"
    | Test (f, v) ->
        let fs = Util.Field.to_string f in
        let vs = Util.Value.to_string v in
        Printf.sprintf "(%s = %s)" fs vs
    | Or   (a, b) -> Printf.sprintf "(%s ∨ %s)" (to_string a) (to_string b)
    | And  (a, b) -> Printf.sprintf "(%s ∧ %s)" (to_string a) (to_string b)
    | Not   a     -> Printf.sprintf "¬(%s)" (to_string a)

  let rec compile (pred: t) : Ast.Term.t =
    match pred with
    | One         -> Ast.Term.one
    | Zero        -> Ast.Term.zero
    | Test (f, v) -> Ast.Term.test f v
    | Or   (a, b) -> Ast.Term.plus (Ast.TermSet.of_list [compile a; compile b])
    | And  (a, b) -> Ast.Term.times [compile a; compile b]
    | Not   a     -> Ast.Term.not (compile a)
end

module Query = struct
  type t =
    | Pred of Predicate.t
    | Plus of t * t
    | Times of t * t
    | Star of t

  let rec to_string query =
    match query with
    | Pred   a      -> Predicate.to_string a
    | Plus  (q, q') -> Printf.sprintf "(%s + %s)" (to_string q) (to_string q')
    | Times (q, q') -> Printf.sprintf "(%s * %s)" (to_string q) (to_string q')
    | Star   q      -> Printf.sprintf "(%s)*" (to_string q)

  let rec compile (p: Ast.Term.t) (t: Ast.Term.t) (query: t) : Ast.Term.t =
    let c = compile p t in
    match query with
    | Pred   a      -> Ast.Term.times [t; Predicate.compile a; p]
    | Plus  (q, q') -> Ast.Term.plus (Ast.TermSet.of_list [c q; c q'])
    | Times (q, q') -> Ast.Term.times [c q; c q']
    | Star   q      -> Ast.Term.star (c q)
end

type network = {
  ingress:  Ast.Term.t; (* in  *)
  outgress: Ast.Term.t; (* out *)
  p:        Ast.Term.t; (* p   *)
  t:        Ast.Term.t; (* t   *)
}

let compile {ingress; outgress; p; t} q =
  Ast.Term.times [ingress; p; (Query.compile p t q); outgress]
