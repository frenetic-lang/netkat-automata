
module D = Decide_Kostas.KostasDeriv
module S = Sexplib.Sexp
module Term = Decide_Kostas.Term
                
open Core.Std
open Sexplib.Conv

module PCC (D : Decide_Kostas.DerivTerm) = struct
  module UF = Decide_Util.UnionFind(Decide_Kostas.Term)

  type certificate = {
    lhs : Term.t;
    rhs : Term.t;
    left_e_matrix : D.EMatrix.t;
    left_d_matrix : D.DMatrix.t;
    right_e_matrix : D.EMatrix.t;
    right_d_matrix : D.DMatrix.t;
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


  let equivalent bisim elm1 elm2 =
    let deriv1 = D.make_term elm1 in
    let deriv2 = D.make_term elm2 in
    D.EMatrix.compare (D.get_e deriv1) (D.get_e deriv2) = 0 && D.DMatrix.equivalent bisim (D.get_d deriv1) (D.get_d deriv2)

  let verify_bisimulation uf t1 t2 =
    (* Check t1 UF t2 *)
    List.for_all (UF.equivalence_classes uf)
      ~f:(fun cls ->
          List.for_all (UF.Class.members cls) (fun mem -> equivalent (UF.eq uf) (UF.Class.canonical_element cls) mem))

  let parse_certificate file =
    let file = S.load_sexp file in
    let cert = certificate_of_sexp file in
    UF.validate cert.bisim;
    let lhs_deriv = D.make_term cert.lhs in
    let rhs_deriv = D.make_term cert.rhs in
    assert (D.get_e lhs_deriv = cert.left_e_matrix);
    assert (D.get_d lhs_deriv = cert.left_d_matrix);
    assert (D.get_e rhs_deriv = cert.right_e_matrix);
    assert (D.get_d rhs_deriv = cert.right_d_matrix)
end
