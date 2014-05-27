
(* SPINES *)

  
module TermMap = Map.Make(struct 
  type t = Decide_Ast.Term.t
  let compare = Decide_Ast.Term.compare
end
)

module TermSet = Decide_Ast.TermSet

type term = Decide_Ast.term

module TermPairSet = struct 
  include Set.Make(struct 
    type t = term * term 
    let compare (al,ar) (bl,br) = 
      match Decide_Ast.Term.compare al bl with 
	| 0 -> Decide_Ast.Term.compare ar br
	| o -> o 
  end)
  let map f ts = 
    fold (fun (l,r) acc -> add (f (l,r)) acc) ts empty
  let bind ts f = 
    fold (fun (l,r) t -> union (f (l,r)) t) ts empty
end
  
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


  
let rec lrspines (e : term) =
  match e with
    | Dup _ -> TermPairSet.singleton (Decide_Ast.make_one, Decide_Ast.make_one)
    | Times (_,l,_) ->
    (match l with
      | [] -> TermPairSet.empty
      | [d] -> lrspines d
      | d :: t ->
	let u = lrspines d in
	let v = lrspines (Decide_Ast.make_times t) in
	let f (l,r) = 
	  l, Decide_Ast.simplify (Decide_Ast.make_times (r :: t)) in
	let r = TermPairSet.map f u in
	let g (l,r) = (Decide_Ast.simplify (Decide_Ast.make_times [d;l]),r) in
	let s = TermPairSet.map g v in
      TermPairSet.union r s)
    | Star (_,d,_) ->
      let s = lrspines d in
      let f (l,r) = Decide_Ast.simplify (Decide_Ast.make_times [e;l]),
	Decide_Ast.simplify (Decide_Ast.make_times [r;e]) in
      TermPairSet.map f s
    | Plus (_,ts,_) -> 
      TermSet.fold (fun x t -> TermPairSet.union (lrspines x) t) ts TermPairSet.empty
    | (Assg _ | Test _ | Not _ | Zero _ | One _) -> TermPairSet.empty      
      
  (* get all lrspines of e and all lrspines of rspines of e *)
  let allLRspines (e :  term) : TermPairSet.t TermMap.t =
    Printf.printf "getting all spines of: %s\n" (Decide_Ast.Term.to_string e);
    if Decide_Util.debug_mode
    then (
      assert (Decide_Ast.all_caches_empty e);
      assert (Decide_Ast.all_ids_assigned e);
    );
    
    let allLR = TermSet.add e (rspines e) in
    let h = ref TermMap.empty in
    let f d = h := TermMap.add d (lrspines d) !h in
    TermSet.iter f allLR; !h
      
  (* (* remove dups of lspines *)                                            *)
  (* let remove_dups_from_Lspines (h : (term, (term_set)) Hashtbl.t) : = *)
  (*   let f x = match x with                                                *)
  (*   | Times [l;r] -> Times [Decide_Ast.zero_dups l; r]                           *)
  (*   | _ -> failwith "remove_dups_from_Lspines" in                         *)
  (*   let g ts = TermSet.map f ts in                                        *)
      
(*
  let display_lrspines (ts : (term_set)) : =
    let f x = match x with
      | Term.Times (_,[l;r],_) -> Printf.printf "%s ||| %s\n" 
	(Term.to_string l) (Term.to_string r)
      | _ -> failwith "display_lrspines" in
    TermSet.iter f ts
*)
