open Decide_Ast
open Decide_Ast.Term

(* SPINES *)

module TermMap = Map.Make(Term)


let make_term = InitialTerm.to_term

(* right spines of a term *)
  let rspines (e : term) : TermSet.t =
    let open InitialTerm in 
    let rec sp (e : InitialTerm.t) : InitialTermSet.t =
      match e with
	| Dup  -> InitialTermSet.return One
	| Plus ts -> InitialTermSet.bind ts sp
	| Times l ->
          (match l with
            | [] -> InitialTermSet.empty
            | [d] -> sp d
            | d :: t ->
              let u = sp d in
              let v = sp (Times t) in
              let s = InitialTermSet.map (fun x -> Times (x :: t)) u in
              InitialTermSet.union s v)
	| Star d ->
          let s = sp d in
          InitialTermSet.map (fun x -> Times [x; e]) s
	| (Assg _ | Test _ | Not _ | Zero  | One  ) -> InitialTermSet.empty in
    InitialTermSet.fold (fun sp acc -> TermSet.add (make_term sp) acc) 
      (InitialTermSet.map simplify_tt (sp (InitialTerm.of_term e))) TermSet.empty
      
(* left spines of a term *)
  let lspines (e : term) : TermSet.t =
    let open InitialTerm in 
    let rec sp (e : InitialTerm.t) : InitialTermSet.t =
      match e with
	| Dup -> InitialTermSet.return One
	| Plus ts -> InitialTermSet.bind ts sp
	| Times l ->
          (match l with
            | [] -> InitialTermSet.empty
            | [d] -> sp d
            | d :: t ->
              let u = sp d in
              let v = sp (Times t) in
              let s = InitialTermSet.map (fun x -> Times [d; x]) v in
              InitialTermSet.union u s)
	| Star d ->
          let s = sp d in
          InitialTermSet.map (fun x -> Times [e; x]) s
	| (Assg _ | Test _ | Not _ | Zero | One) -> InitialTermSet.empty in
    
    InitialTermSet.fold (fun sp acc -> TermSet.add (make_term sp) acc)
	(InitialTermSet.map simplify_tt (sp (InitialTerm.of_term e))) TermSet.empty

      
  let lrspines (e : term) : TermSet.t =
    let open InitialTerm in 
    let rec lrspines e = 
      match e with
	| Dup -> InitialTermSet.return (Times [One; One])
	| Plus ts -> InitialTermSet.bind ts lrspines
	| Times l ->
	  (match l with
	    | [] -> InitialTermSet.empty
	    | [d] -> lrspines d
	    | d :: t ->
	      let u = lrspines d in
	      let v = lrspines (Times t) in
	      let f x = match x with
		| Times [l;r] -> Times [l; simplify_tt (Times (r :: t))]
		| _ -> failwith "lrspines 1" in
	      let r = InitialTermSet.map f u in
	      let g x = match x with
		| Times [l;r] -> Times [simplify_tt (Times [d;l]); r]
		| _ -> failwith "lrspines 2" in
	      let s = InitialTermSet.map g v in
	      InitialTermSet.union r s)
	| Star d ->
	  let s = lrspines d in
	  let f x = match x with
	    | Times [l;r] -> Times [simplify_tt (Times [e;l]); 
				    simplify_tt (Times [r;e])]
	    | _ -> failwith "lrspines 3" in
	  InitialTermSet.map f s
	| (Assg _ | Test _ | Not _ | Zero | One) -> InitialTermSet.empty
    in InitialTermSet.fold (fun x -> TermSet.add (make_term x)) (lrspines (InitialTerm.of_term e)) TermSet.empty
	
  (* get all lrspines of e and all lrspines of rspines of e *)
  let allLRspines (e : term) : TermSet.t TermMap.t =
    let allLR = TermSet.add e (rspines e) in
    let h = ref TermMap.empty in
    let f d = h := TermMap.add d (lrspines d) !h in
    TermSet.iter f allLR; !h
      
  (* (* remove dups of lspines *)                                            *)
  (* let remove_dups_from_Lspines (h : (term, TermSet.t) Hashtbl.t) : unit = *)
  (*   let f x = match x with                                                *)
  (*   | Times [l;r] -> Times [Decide_Ast.zero_dups l; r]                           *)
  (*   | _ -> failwith "remove_dups_from_Lspines" in                         *)
  (*   let g ts = TermSet.map f ts in                                        *)
      
  let display_lrspines (ts : TermSet.t) : unit =
    let f x = match x with
      | Times (_,[l;r]) -> Printf.printf "%s ||| %s\n" 
	(Term.to_string l) (Term.to_string r)
      | _ -> failwith "display_lrspines" in
    TermSet.iter f ts
