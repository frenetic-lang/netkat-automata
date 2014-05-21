
(* SPINES *)

module Spines = functor(U : Decide_Base.UnivDescr) -> struct 

  module Cached = Decide_Ast_Cache.Ast(U)
    
module TermMap = Map.Make(struct 
  type t = unit Decide_Ast.Term.t
  let compare = Decide_Ast.Term.compare
end
)

module TermSet = struct 
  open Decide_Ast
  type t = unit TermSet.t
  let singleton = TermSet.singleton
  let empty = TermSet.empty ()
  let add = TermSet.add 
  let map = TermSet.map
  let fold = TermSet.fold 
  let union = TermSet.union 
  let bind = TermSet.bind
  let iter = TermSet.iter
end

type term = unit Decide_Ast.term

open Decide_Ast.Term

let rspines (e : term) : TermSet.t =
  let rec sp (e : term) : TermSet.t =
      match e with
	| Dup _ -> TermSet.singleton Decide_Ast.make_one
	| Plus (_,ts,_) -> TermSet.bind ts sp
	| Times (_,l,_) ->
        (match l with
          | [] -> TermSet.empty
          | [d] -> sp d
          | d :: t ->
            let u = sp d in
            let v = sp (Decide_Ast.make_times t) in
            let s = TermSet.map (fun x -> Decide_Ast.make_times (x :: t)) u in
          TermSet.union s v)
	| Star (_,d,_) ->
          let s = sp d in
        TermSet.map (fun x -> Decide_Ast.make_times [x; e]) s
	| (Assg _ | Test _ | Not _ | Zero _ | One _) -> TermSet.empty in
  TermSet.map Decide_Ast.simplify (sp e)
  
let rec lrspines (e : term) : TermSet.t =
  match e with
    | Dup _ -> TermSet.singleton (Decide_Ast.make_times [Decide_Ast.make_one; Decide_Ast.make_one])
    | Plus (_,ts,_) -> TermSet.bind ts lrspines
    | Times (_,l,_) ->
    (match l with
      | [] -> TermSet.empty
      | [d] -> lrspines d
      | d :: t ->
	let u = lrspines d in
	let v = lrspines (Decide_Ast.make_times t) in
	let f x = match x with
	  | Times (_,[l;r],_) -> Decide_Ast.make_times [l; Decide_Ast.simplify (Decide_Ast.make_times (r :: t))]
	  | _ -> failwith "lrspines 1" in
	let r = TermSet.map f u in
	let g x = match x with
	  | Times (_,[l;r],_) -> Decide_Ast.make_times [Decide_Ast.simplify (Decide_Ast.make_times [d;l]); r]
	  | _ -> failwith "lrspines 2" in
	let s = TermSet.map g v in
      TermSet.union r s)
    | Star (_,d,_) ->
      let s = lrspines d in
      let f x = match x with
	| Times (_,[l;r],_) -> Decide_Ast.make_times [Decide_Ast.simplify (Decide_Ast.make_times [e;l]);
				Decide_Ast.simplify (Decide_Ast.make_times [r;e])]
	| _ -> failwith "lrspines 3" in
      TermSet.map f s
    | (Assg _ | Test _ | Not _ | Zero _ | One _) -> TermSet.empty      
      
  (* get all lrspines of e and all lrspines of rspines of e *)
  let allLRspines (e :  term) : (unit term_set) TermMap.t =
    Printf.printf "getting all spines of: %s\n" (Decide_Ast.Term.to_string e);
    if Decide_Util.debug_mode
    then (
      assert (Cached.all_caches_empty e);
      assert (Decide_Ast.all_ids_assigned e);
    );
    
    let allLR = TermSet.add e (rspines e) in
    let h = ref TermMap.empty in
    let f d = h := TermMap.add d (lrspines d) !h in
    TermSet.iter f allLR; !h
      
  (* (* remove dups of lspines *)                                            *)
  (* let remove_dups_from_Lspines (h : (term, (unit term_set)) Hashtbl.t) : unit = *)
  (*   let f x = match x with                                                *)
  (*   | Times [l;r] -> Times [Decide_Ast.zero_dups l; r]                           *)
  (*   | _ -> failwith "remove_dups_from_Lspines" in                         *)
  (*   let g ts = TermSet.map f ts in                                        *)
      
(*
  let display_lrspines (ts : (unit term_set)) : unit =
    let f x = match x with
      | Term.Times (_,[l;r],_) -> Printf.printf "%s ||| %s\n" 
	(Term.to_string l) (Term.to_string r)
      | _ -> failwith "display_lrspines" in
    TermSet.iter f ts
*)
end
