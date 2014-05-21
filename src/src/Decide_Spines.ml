open Decide_Ast

(* SPINES *)

module Spines = functor(U : Decide_Base.UnivDescr) -> struct 
    
  module Cached = Decide_Ast_Cache.Ast(U)

module TermMap = Map.Make(Cached.Term)

type extra = Cached.cached_info

module TermSet = struct 
  type t = extra TermSet.t
  let singleton = TermSet.singleton
  let empty = TermSet.empty ()
  let add = TermSet.add 
  let map = TermSet.map
  let fold = TermSet.fold 
  let union = TermSet.union 
  let bind = TermSet.bind
  let iter = TermSet.iter
end


(* right spines of a term *)
  let rspines (e : extra term) : (extra term_set) =
    let open Term in 
    let rec sp (e : (extra term)) : (extra term_set) =
      match e with
	| Dup _ -> TermSet.singleton Cached.one
	| Plus (_,ts,_) -> TermSet.bind ts sp
	| Times (_,l,_) ->
          (match l with
            | [] -> TermSet.empty
            | [d] -> sp d
            | d :: t ->
              let u = sp d in
              let v = sp (Cached.times t) in
              let s = TermSet.map (fun x -> Cached.times (x :: t)) u in
              TermSet.union s v)
	| Star (_,d,_) ->
          let s = sp d in
          TermSet.map (fun x -> Cached.times [x; e]) s
	| (Assg _ | Test _ | Not _ | Zero _  | One _  ) -> TermSet.empty in
    TermSet.fold (fun sp acc -> TermSet.add sp acc) 
      (sp e) TermSet.empty
      
  let lrspines (e : extra term) : (extra term_set) =
    let open Term in 
    let rec lrspines (e : extra term) = 
      match e with
	| Dup _ -> TermSet.singleton (Cached.times [Cached.one; Cached.one])
	| Plus (_,ts,_) -> TermSet.bind ts lrspines
	| Times (_,l,_) ->
	  (match l with
	    | [] -> TermSet.empty
	    | [d] -> lrspines d
	    | d :: t ->
	      let u = lrspines d in
	      let v = lrspines (Cached.times t) in
	      let f x = match x with
		| Times (_,[l;r],_) -> Cached.times [l; (Cached.times (r :: t))]
		| _ -> failwith "lrspines 1" in
	      let r = TermSet.map f u in
	      let g x = match x with
		| Times (_,[l;r],_) -> Cached.times [(Cached.times [d;l]); r]
		| _ -> failwith "lrspines 2" in
	      let s = TermSet.map g v in
	      TermSet.union r s)
	| Star (_,d,_) ->
	  let s = lrspines d in
	  let f x = match x with
	    | Times (_,[l;r],_) -> Cached.times [(Cached.times [e;l]); 
				    (Cached.times [r;e])]
	    | _ -> failwith "lrspines 3" in
	  TermSet.map f s
	| (Assg _ | Test _ | Not _ | Zero _ | One _) -> TermSet.empty
    in TermSet.fold (fun x -> TermSet.add x) (lrspines e) TermSet.empty
	
  (* get all lrspines of e and all lrspines of rspines of e *)
  let allLRspines (e : extra term) : (extra term_set) TermMap.t =
    let allLR = TermSet.add e (rspines e) in
    let h = ref TermMap.empty in
    let f d = h := TermMap.add d (lrspines d) !h in
    TermSet.iter f allLR; !h
      
  (* (* remove dups of lspines *)                                            *)
  (* let remove_dups_from_Lspines (h : (term, (extra term_set)) Hashtbl.t) : extra = *)
  (*   let f x = match x with                                                *)
  (*   | Times [l;r] -> Times [Decide_Ast.zero_dups l; r]                           *)
  (*   | _ -> failwith "remove_dups_from_Lspines" in                         *)
  (*   let g ts = TermSet.map f ts in                                        *)
      
(*
  let display_lrspines (ts : (extra term_set)) : extra =
    let f x = match x with
      | Term.Times (_,[l;r],_) -> Printf.printf "%s ||| %s\n" 
	(Term.to_string l) (Term.to_string r)
      | _ -> failwith "display_lrspines" in
    TermSet.iter f ts
*)
end
