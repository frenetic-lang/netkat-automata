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
    | Pred of Predicate.t * Predicate.t
    | Plus of t * t
    | Times of t * t
    | Star of t

  let rec to_string query =
    match query with
    | Pred  (a, b)  -> Printf.sprintf "(%s, %s)"
                       (Predicate.to_string a) (Predicate.to_string b)
    | Plus  (q, q') -> Printf.sprintf "(%s + %s)" (to_string q) (to_string q')
    | Times (q, q') -> Printf.sprintf "(%s ; %s)" (to_string q) (to_string q')
    | Star   q      -> Printf.sprintf "(%s)*" (to_string q)

  let rec compile (p: Ast.Term.t) (t: Ast.Term.t) (query: t) : Ast.Term.t =
    let c = compile p t in
    match query with
    | Pred  (a, b)  -> Ast.Term.times [p; Predicate.compile a; t; Predicate.compile b]
    | Plus  (q, q') -> Ast.Term.plus (Ast.TermSet.of_list [c q; c q'])
    | Times (q, q') -> Ast.Term.times [c q; c q']
    | Star   q      -> Ast.Term.star (c q)
end

let uncurry f (a, b) =
  f a b

let term_of_field_value ((f, v): (string * string)) : (Util.Field.t * Util.Value.t) =
  (Util.Field.of_string f, Util.Value.of_string v)

let term_of_location (l: Frenetic_NetKAT.location) : (string * string) =
  let open Frenetic_NetKAT in
  match l with
  | Physical i  -> ("port",  Int32.to_string i)
  | FastFail is -> ("ports", string_of_fastfail is)
  | Pipe s      -> ("port",  "pipe(" ^ s ^ ")")
  | Query s     -> ("port",  "query(" ^ s ^ ")")

let term_of_header_val (h: Frenetic_NetKAT.header_val) : (string * string) =
  let open Frenetic_NetKAT in
  let open Frenetic_Packet in
  match h with
  | Switch i         -> ("switch",     Int64.to_string i)
  | Location l       -> term_of_location l
  | EthSrc addr      -> ("ethSrc",     string_of_mac addr)
  | EthDst addr      -> ("ethDst",     string_of_mac addr)
  | Vlan i           -> ("vlanId",     string_of_int i)
  | VlanPcp i        -> ("vlanPcp",    string_of_int i)
  | EthType i        -> ("ethType",    string_of_int i)
  | IPProto i        -> ("ipProto",    string_of_int i)
  | IP4Src (addr, i) -> ("ip4src",     string_of_ip addr ^ "/" ^ Int32.to_string i)
  | IP4Dst (addr, i) -> ("ip4dst",     string_of_ip addr ^ "/" ^ Int32.to_string i)
  | TCPSrcPort i     -> ("tcpSrcPort", string_of_int i)
  | TCPDstPort i     -> ("tcpDstPort", string_of_int i)
  | VSwitch i        -> ("vswitch",    Int64.to_string i)
  | VPort i          -> ("vport",      Int64.to_string i)
  | VFabric i        -> ("vfabric",    Int64.to_string i)

let rec term_of_pred (p: Frenetic_NetKAT.pred) : Ast.Term.t =
  let open Ast in
  let open Frenetic_NetKAT in
  match p with
  | True       -> Term.one
  | False      -> Term.zero
  | Test h     -> uncurry Term.test (term_of_field_value (term_of_header_val h))
  | And (a, b) -> Term.times [term_of_pred a; term_of_pred b]
  | Or (a, b)  -> Term.plus (TermSet.of_list [term_of_pred a; term_of_pred b])
  | Neg a      -> Term.not (term_of_pred a)

let rec term_of_policy p =
  let open Ast in
  let open Frenetic_NetKAT in
  match p with
  | Filter a     -> term_of_pred a
  | Mod h        -> uncurry Term.assg (term_of_field_value (term_of_header_val h))
  | Union (p, q) -> Term.plus (TermSet.of_list [term_of_policy p; term_of_policy q])
  | Seq (p, q)   -> Term.times [term_of_policy p; term_of_policy q]
  | Star p       -> Term.star (term_of_policy p)
  | Link  (_s1, _p1, _s2, _p2) -> failwith "Link not yet implemented"
  | VLink (_s1, _p1, _s2, _p2) -> failwith "VLink not yet implemented"

type network = {
  ingress:  Ast.Term.t;
  outgress: Ast.Term.t;
  p:        Ast.Term.t;
  t:        Ast.Term.t;
}

let compile {ingress; outgress; p; t} q =
  Ast.Term.times [ingress; (Query.compile p t q); p; outgress]
