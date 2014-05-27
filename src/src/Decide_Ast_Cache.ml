
module Ast = functor (U : Decide_Base.UnivDescr) -> struct

  module Univ = Decide_Base.Univ(U)

  type cached_info = { e_matrix : unit -> Univ.Base.Set.t;
		       one_dup_e_matrix : unit -> Univ.Base.Set.t}

  module T = Decide_Ast
    
  module Term = struct 
    module T = T.Term
    type t = cached_info T.t
    let compare : t -> t -> int = T.compare
  end
    

  let get_cache_option t :  'a option = 
    let open Decide_Ast.Term in 
    match t with 
      | Assg (_,_,_,c) -> c
      | Test (_,_,_,c) -> c
      | Dup (_,c) -> c
      | Plus (_,_,c) -> c
      | Times (_,_,c) -> c
      | Not (_,_,c) -> c
      | Star (_,_,c) -> c
      | Zero (_,c) -> c
      | One (_,c) -> c

  open Univ.Base
  open Univ.Base.Set

  let get_cache t :  cached_info  = 
    match get_cache_option t with 
      | Some c -> c
      | None -> failwith "can't extract; caache not set"
    
  let thunkify f = 
    let ret = ref None in 
    (fun _ -> 
      match !ret with 
	| None -> let v = f() in ret := Some v; v
	| Some v -> v)

  let of_plus ts = 
    thunkify (fun _ -> Decide_Ast.TermSet.fold 
      (fun t acc -> union ((get_cache t).e_matrix ()) acc) ts empty )

  let of_plus_onedup ts = 
    thunkify (fun _ -> Decide_Ast.TermSet.fold 
      (fun t acc -> union ((get_cache t).one_dup_e_matrix ()) acc) ts empty)
   
  let of_times tl = 
    thunkify (fun _ -> List.fold_right (fun t acc ->  mult ((get_cache t).e_matrix ()) acc) tl
      (singleton univ_base) )

  let of_times_onedup tl = 
    thunkify (fun _ -> List.fold_right (fun t acc ->  mult ((get_cache t).one_dup_e_matrix ()) acc) tl
      (singleton univ_base) )

  let rec all_caches_empty t = 
    let open Decide_Ast.Term in 
    match t with 
      | (Assg _ | Test _ | Zero _ | One _ | Dup _) -> 
	(match get_cache_option t with None -> true | Some _ -> false)
      | Plus(_,x,c) -> 
	(match c with None -> true | Some _ -> false) && 
	  (Decide_Ast.TermSet.fold 
	     (fun e acc -> all_caches_empty e && acc) x true)
      | Times(_,x,c) -> 
	(match c with None -> true | Some _ -> false) && 
	  (List.for_all (fun e -> all_caches_empty e) x)
      | (Not (_,x,c) | Star (_,x,c)) -> 
	(match c with None -> true | Some _ -> false) && (all_caches_empty x)


  let rec set_cache (t : 'a Decide_Ast.term) (o : 'b) : 'b Decide_Ast.term = 
    let open Decide_Ast.Term in 
	match t with 
	  | Assg (a,b,c,_) -> Assg(a,b,c,Some o)
	  | Test (a,b,c,_) -> Test(a,b,c,Some o)
	  | Dup (a,_) -> Dup(a,Some o)
	  | Plus (a,b,_) -> Plus (a,Decide_Ast.TermSet.map (fun x -> set_cache x o) b,Some o)
	  | Times (a,b,_) -> Times (a,List.map (fun x -> set_cache x o) b,Some o)
	  | Not (a,b,_) -> Not (a,set_cache b o,Some o)
	  | Star (a,b,_) -> Star (a,set_cache b o,Some o)
	  | Zero (a,_) -> Zero(a,Some o)
	  | One (a,_) -> One(a,Some o)


  let add_cache (on_hit : 'a Decide_Ast.term -> cached_info Decide_Ast.term) (t : 'a Decide_Ast.term) : cached_info Decide_Ast.term 
      = 
    let rec of_term t0 = (
      (* to negate a test x=v, we allow x to have any value except v *)
      let negate (x : U.field) (v : U.value) : Univ.Base.Set.t =
	Univ.Base.Set.singleton(Univ.Base.of_neg_test x v)
      in
      let open Decide_Ast.Term in 
	  match get_cache_option t with 
	    | Some c -> on_hit t
	    | None -> 
	      begin 
		match t0 with 
		  | One (id,_) -> 
		    One (id,Some {e_matrix = (fun _ -> singleton univ_base);
				  one_dup_e_matrix = (fun _ -> singleton univ_base)})
		  | Zero (id,_) -> 
		    Zero(id,Some {e_matrix = (fun _ -> empty);
				  one_dup_e_matrix = (fun _ -> empty)})
		  | Assg(id,field,v,_) -> 
		    let r = thunkify (fun _ -> singleton (of_assg field v)) in
		    Assg(id,field,v,Some {e_matrix = r; one_dup_e_matrix = r})
		  | Test(id,field,v,_) ->  
		    let r = thunkify (fun _ -> singleton (of_test field v)) in
		    Test(id,field,v, Some {e_matrix = r; one_dup_e_matrix = r})
		  | Dup (id,_) -> 
		    Dup(id,Some {e_matrix = (fun _ -> empty); one_dup_e_matrix = (fun _ -> singleton univ_base)})
		  | Plus (id,ts,_) ->
		    let ts = Decide_Ast.TermSet.map of_term ts in
		    Plus(id,ts,Some {e_matrix = of_plus ts; one_dup_e_matrix = of_plus_onedup ts})
		  | Times (id,tl,_) -> 
		    let tl = List.map of_term tl in 
		    Times(id,tl,Some { e_matrix = of_times tl; one_dup_e_matrix  = of_times_onedup tl})
		  | Not (id,x,_) -> 
		    let x = of_term x in 
		    let m = thunkify (fun _ -> match x with
		      | Zero _ -> singleton univ_base
		      | One _ -> empty
		      | Test (_,x,v,_) -> negate x v
		      | _ -> failwith "De Morgan law should have been applied") in 
		    Not(id,x,Some {e_matrix = m; one_dup_e_matrix = m})
		  | Star (id,x,_) ->
		    let x = of_term x in
		    let get_fixpoint s = 
		      Printf.printf "getting fixpoint...\n%!";
		      let s1 = add univ_base s in
		      let rec f s r  =
			if equal s r then (Printf.printf "got fixpoint!\n%!"; s)
			else f (mult s s) s in
		      f (mult s1 s1) s1 in 
		    let me = thunkify (fun _ -> get_fixpoint ((get_cache x).e_matrix())) in 
		    let mo = thunkify (fun _ -> get_fixpoint ((get_cache x).one_dup_e_matrix())) in
		    Star(id,x,Some {e_matrix = me; one_dup_e_matrix = mo}) end ) in 
    of_term t

  let remove_cache (t :  'a Decide_Ast.term) = 
    let open Decide_Ast.Term in 
	match t with 
	  | Assg (a,b,c,_) -> Assg(a,b,c,None)
	  | Test (a,b,c,_) -> Test(a,b,c,None)
	  | Dup (a,_) -> Dup(a,None)
	  | Plus (a,b,_) -> Plus (a,b,None)
	  | Times (a,b,_) -> Times (a,b,None)
	  | Not (a,b,_) -> Not (a,b,None)
	  | Star (a,b,_) -> Star (a,b,None)
	  | Zero (a,_) -> Zero(a,None)
	  | One (a,_) -> One(a,None)

  let remove_and_readd (x : 'a Decide_Ast.term) : cached_info Decide_Ast.term = 
    (add_cache (fun _ -> failwith "removed the cache!") (remove_cache x))

  let of_term (t : 'a Decide_Ast.term) : cached_info Decide_Ast.term = add_cache remove_and_readd t

  let refresh_cache t = add_cache (fun x -> x) t
    
  open Decide_Ast.Term

let zero : Term.t = 
  match Decide_Ast.make_zero with 
    | Zero(id,None) -> Zero(id,Some {e_matrix = (fun _ -> empty); one_dup_e_matrix = (fun _ -> empty)})
    | _ -> failwith "bug in make_zero"

let one : Term.t = 
  match Decide_Ast.make_one with 
    | One(id,None) -> One(id,Some {e_matrix = (fun _ -> singleton univ_base); one_dup_e_matrix = (fun _ -> singleton univ_base)})
    | _ -> failwith "bug in make_one"

let plus (l : cached_info Decide_Ast.TermSet.t) : Term.t = 
  match Decide_Ast.TermSet.elements l with 
    | [x] -> x 
    | _ -> 
      begin 
	match Decide_Ast.make_plus (Decide_Ast.TermSet.fold (fun e a -> e::a) l []) with 
	  | Plus(id,l,None) -> 
	    Plus(id,l,Some {e_matrix = of_plus l; one_dup_e_matrix = of_plus_onedup l})
	  | _ -> failwith "bug in make_plus"
      end

let times (l : Term.t list) : Term.t = 
  match l with 
    | [x] -> x
    | _ -> refresh_cache (Decide_Ast.make_times l)

end
