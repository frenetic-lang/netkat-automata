
open Decide_Util

module EquivChecker (DerivTerm : Decide_Deriv.DerivTerm) = struct    
  module Ast = Decide_Ast
  module Deriv = Decide_Deriv

  module WorkList = WorkList(struct 
      type t = (DerivTerm.t * DerivTerm.t) 
      let compare = (fun (a1,b1) (a2,b2) -> 
          match (DerivTerm.compare a1 a2) with 
          | 0 -> (DerivTerm.compare b1 b2)
          | k -> k)
    end)

  let print_wl_pair (a,b)= Printf.sprintf "%s\n%s" (DerivTerm.to_string a) (DerivTerm.to_string b)
  module PCC = Decide_PCC.PCC(DerivTerm)
  module UF = Decide_Util.UnionFind(DerivTerm)

  let check_equivalent (t1: Ast.Term.t) (t2: Ast.Term.t) : bool = 

    let bisim = UF.create ()
    in

    let rec main_loop work_list = 
      if WorkList.is_empty work_list
      then
        true
      else
        let q1,q2 = WorkList.hd work_list in
        Printf.printf "q1: %s\nq2: %s\n" (DerivTerm.to_string q1) (DerivTerm.to_string q2);
        let rest_work_list = WorkList.tl work_list in
        let q1_E = DerivTerm.get_e q1 in 
        let q2_E = DerivTerm.get_e q2 in 
        if not (DerivTerm.EMatrix.compare q1_E q2_E = 0)
        then false
        else
          let u,f = UF.find bisim q1, UF.find bisim q2 in
          (* find e returns the canonical element for e, so equality of classes
             is just equality of canonical elements *)
          if  DerivTerm.compare u f = 0
          then main_loop rest_work_list
          else 
            (let _ = UF.union bisim u f in
             let q1d = DerivTerm.get_d q1 in 
             let q2d = DerivTerm.get_d q2 in 
             let work_list = DerivTerm.EMatrix.fold
                 (DerivTerm.EMatrix.union (DerivTerm.DMatrix.points q1d) (DerivTerm.DMatrix.points q2d))
                 ~init:rest_work_list
                 ~f:(fun expanded_work_list pt -> 
                     let q1' = DerivTerm.DMatrix.run q1d pt in
                     let q2' = DerivTerm.DMatrix.run q2d pt in
                     WorkList.add (DerivTerm.make_term q1',DerivTerm.make_term q2')
                       expanded_work_list)
             in
             main_loop work_list) in

    let t2' = DerivTerm.make_term (Ast.TermSet.singleton t2) in

    let t1' = DerivTerm.make_term (Ast.TermSet.singleton t1) in

    let ret = main_loop (WorkList.singleton (t1',t2')) in
    PCC.generate_certificate t1 t2 t1' t2' bisim;
    Decide_Util.print_debugging_info ();
    ret

  let check_certificate = PCC.parse_certificate

  module TermList = Decide_Util.WorkList(struct
      type t = DerivTerm.t
      let compare = DerivTerm.compare
    end)

  module DerivTermSet = Set.Make (struct
      type t = DerivTerm.t with sexp, compare
    end)

  module TermSetSet = Set.Make (struct
      type t = Ast.TermSet.t with sexp, compare
    end)

  let check_eval t =

    let rec main_loop work_list seen =
      if TermList.is_empty work_list
      then
        ()
      else
        let t = TermList.hd work_list in
        let rest_work_list = TermList.tl work_list in
        Printf.printf "t: %s\n" (DerivTerm.to_string t);
        let t_D = DerivTerm.get_d t in
        let work_list,seen = DerivTerm.EMatrix.fold
            (DerivTerm.DMatrix.points t_D)
            ~init:(rest_work_list, seen)
            ~f:(fun (expanded_work_list,seen) pt -> 
                let t' = DerivTerm.DMatrix.run t_D pt in
                if TermSetSet.mem t' seen
                then
                  expanded_work_list, seen
                else
                  TermList.add (DerivTerm.make_term t')
                    expanded_work_list, TermSetSet.add t' seen) in
        main_loop work_list seen in

    main_loop (TermList.singleton (DerivTerm.make_term (Ast.TermSet.singleton t))) (TermSetSet.singleton (Ast.TermSet.singleton t))
end
