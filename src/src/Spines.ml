open Ast
open Ast.Term

(* right spines of a term *)
let rspines (e : term) : TermSet.t =
  let rec sp (e : term) : TermSet.t =
      match e with
      | Dup -> TermSet.return One
      | Plus ts -> TermSet.bind ts sp
      | Times l ->
        (match l with
        | [] -> TermSet.empty
        | [d] -> sp d
        | d :: t ->
          let u = sp d in
          let v = sp (Times t) in
          let s = TermSet.map (fun x -> Times (x :: t)) u in
          TermSet.union s v)
      | Star d ->
        let s = sp d in
        TermSet.map (fun x -> Times [x; e]) s
      | (Assg _ | Test _ | Not _ | Zero | One) -> TermSet.empty in
  TermSet.map Ast.simplify (sp e)
  
(* left spines of a term *)
let lspines (e : term) : TermSet.t =
  let rec sp (e : term) : TermSet.t =
    match e with
      | Dup -> TermSet.return One
      | Plus ts -> TermSet.bind ts sp
      | Times l ->
        (match l with
          | [] -> TermSet.empty
          | [d] -> sp d
          | d :: t ->
            let u = sp d in
            let v = sp (Times t) in
            let s = TermSet.map (fun x -> Times [d; x]) v in
            TermSet.union u s)
      | Star d ->
        let s = sp d in
        TermSet.map (fun x -> Times [e; x]) s
      | (Assg _ | Test _ | Not _ | Zero | One) -> TermSet.empty in
  TermSet.map Ast.simplify (sp e)
  
let rec lrspines (e : term) : TermSet.t =
  match e with
  | Dup -> TermSet.return (Times [One; One])
  | Plus ts -> TermSet.bind ts lrspines
  | Times l ->
    (match l with
    | [] -> TermSet.empty
    | [d] -> lrspines d
    | d :: t ->
      let u = lrspines d in
      let v = lrspines (Times t) in
      let f x = match x with
      | Times [l;r] -> Times [l; Ast.simplify (Times (r :: t))]
      | _ -> failwith "lrspines 1" in
      let r = TermSet.map f u in
      let g x = match x with
      | Times [l;r] -> Times [Ast.simplify (Times [d;l]); r]
      | _ -> failwith "lrspines 2" in
      let s = TermSet.map g v in
      TermSet.union r s)
  | Star d ->
    let s = lrspines d in
    let f x = match x with
    | Times [l;r] -> Times [Ast.simplify (Times [e;l]); 
			    Ast.simplify (Times [r;e])]
    | _ -> failwith "lrspines 3" in
    TermSet.map f s
  | (Assg _ | Test _ | Not _ | Zero | One) -> TermSet.empty

(* get all lrspines of e and all lrspines of rspines of e *)
let allLRspines (e : term) : (term, TermSet.t) Hashtbl.t =
  let allLR = TermSet.add e (rspines e) in
  let h = Hashtbl.create 11 in
  let f d = Hashtbl.add h d (lrspines d) in
  TermSet.iter f allLR; h
  
(* (* remove dups of lspines *)                                            *)
(* let remove_dups_from_Lspines (h : (term, TermSet.t) Hashtbl.t) : unit = *)
(*   let f x = match x with                                                *)
(*   | Times [l;r] -> Times [Ast.zero_dups l; r]                           *)
(*   | _ -> failwith "remove_dups_from_Lspines" in                         *)
(*   let g ts = TermSet.map f ts in                                        *)

let display_lrspines (ts : TermSet.t) : unit =
  let f x = match x with
  | Times [l;r] -> Printf.printf "%s ||| %s\n" 
    (Ast.term_to_string l) (Ast.term_to_string r)
  | _ -> failwith "display_lrspines" in
  TermSet.iter f ts
  
