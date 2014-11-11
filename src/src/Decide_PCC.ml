
module D = Decide_Kostas.DerivTerm
module S = Sexplib.Sexp
module Term = Decide_Kostas.Term
                
open Core.Std
open Sexplib.Conv

module UF = Decide_Util.UnionFind(Decide_Deriv.DerivTerm)

type certificate = {
  lhs : Term.t;
  rhs : Term.t;
  left_e_matrix : D.e;
  left_d_matrix : D.d;
  right_e_matrix : D.e;
  right_d_matrix : D.d;
  bisim : UF.t
} with sexp

let generate_certificate t1 t2 t1' t2' uf =
  let t = D.make_term t1 in
  let t' = D.make_term t2 in
  let cert = {
    lhs = t1;
    rhs = t2;
    left_e_matrix = D.get_e t;
    right_e_matrix = D.get_e t';
    left_d_matrix = D.get_d t;
    right_d_matrix = D.get_d t';
    bisim = uf
  } in
  let file = Pervasives.open_out "netkat.cert" in
  Printf.fprintf file ";; %s == %s\n"
    (Term.to_string t1)
    (Term.to_string t2);
  Printf.fprintf file "%s" (S.to_string (sexp_of_certificate cert));
  Pervasives.close_out file

let parse_certificate file =
  let file = S.load_sexp file in
  certificate_of_sexp file
