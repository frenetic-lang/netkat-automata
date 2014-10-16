
module D = Decide_Deriv.DerivTerm
module S = Sexplib.Sexp
module Term = Decide_Ast.Term
module Base = Decide_Base.Base
                
open Core.Std
open Sexplib.Conv

module PointMap = Map.Make (struct
    type t = Base.point
    let sexp_of_t = Base.sexp_of_point
    let t_of_sexp = Base.point_of_sexp
    let compare x y = Base.compare_point x y
  end)

type certificate = {
  lhs : Term.t;
  rhs : Term.t;
  left_e_matrix : Base.Set.t;
  left_d_matrix : Term.t PointMap.t;
  right_e_matrix : Base.Set.t;
  right_d_matrix : Term.t PointMap.t
} with sexp

let generate_certificate t1 t2 t1' t2' _ =
  let get_d_matrix t' =
    let terms, base = D.run_d t' in
    Base.Set.fold_points (fun p map -> PointMap.add map ~key:p ~data:(D.get_term (terms p))) base PointMap.empty in
  let cert = {
    lhs = t1;
    rhs = t2;
    left_e_matrix = D.run_e t1';
    right_e_matrix = D.run_e t2';
    left_d_matrix = get_d_matrix t1';
    right_d_matrix = get_d_matrix t2';
  } in
  let file = Pervasives.open_out "netkat.cert" in
  Printf.fprintf file ";; %s == %s\n"
    (Decide_Ast.Term.to_string t1)
    (Decide_Ast.Term.to_string t2);
  Printf.fprintf file "%s" (S.to_string (sexp_of_certificate cert));
  Pervasives.close_out file

let parse_certificate file =
  let file = S.load_sexp file in
  certificate_of_sexp file
  
