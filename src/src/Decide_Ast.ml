
open Decide_Util

exception Empty

let utf8 = ref false 

module UnivMap = SetMapF(Field)(Value)


(***********************************************
 * syntax
 ***********************************************)
module rec Term : sig 
  type d = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One 
  and t = 
      { uid : int;
        desc : d;
        hash : int;
        mutable spines : TermPairSet.t option; 
	e_matrix : unit -> Decide_Base.Base.Set.t;
	one_dup_e_matrix : unit -> Decide_Base.Base.Set.t }

  val make_assg : Field.t * Value.t -> t
  val make_test : Field.t * Value.t -> t
  val make_dup : unit -> t
  val make_plus : TermSet.t -> t
  val make_times : t list -> t
  val make_not : t -> t
  val make_star : t -> t
  val make_zero : unit -> t
  val make_one : unit -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
  val lrspines : t -> TermPairSet.t
  val fields : t -> FieldSet.t
  val values : t -> UnivMap.t
  val one_dup_e_matrix : t -> Decide_Base.Base.Set.t
  val e_matrix : t -> Decide_Base.Base.Set.t
end = struct
  type d = 
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSet.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One 
  and t = 
      { uid : int;        
        desc : d; 
        hash : int;
        mutable spines : TermPairSet.t option; 
	e_matrix : unit -> Decide_Base.Base.Set.t;
	one_dup_e_matrix : unit -> Decide_Base.Base.Set.t
      }

  type this_t = t

  let compare (t1:t) (t2:t) : int = 
    compare t1.uid t2.uid

  let equal (t1:t) (t2:t) = 
    compare t1 t2 = 0

  let hash (t:t) = 
    t.hash
      
  (***********************************************
  * utilities
  ***********************************************)
      
  let rec is_test (t : t) : bool =
    match t.desc with
      | Assg _ -> false
      | Test _ -> true
      | Dup  -> false
      | Times x -> List.for_all is_test x
      | Plus x -> TermSet.for_all is_test x
      | Not x -> is_test x || failwith "May not negate an action"
      | Star x -> is_test x
      | (Zero  | One ) -> true

  let is_times (e : t) = 
    match e.desc with Times _ -> true | _ -> false

  let is_star (e : t) = 
    match e.desc with Star _ -> true | _ -> false

  let is_star_of (e_star : t) (e : t) = 
    match e_star.desc with 
      | Star e' when (compare e e' = 0) -> true 
      | _ -> false

  let has_star tl = List.fold_left (fun acc e -> is_star e || acc) false tl
	
  let fields (t : t) : FieldSet.t =
    let rec fields t = match t.desc with
      | (Assg (x,_) | Test(x,_)) -> [x]
      | Times x -> List.concat (List.map fields x)
      | Plus x -> List.concat (List.map fields (TermSet.elements x))
      | (Not x | Star x) -> fields x
      | (Dup  | Zero  | One ) -> [] in 
    FieldSet.of_list (fields t)
	
  (* Collect the possible values of each variable *)
  let values (t : t) : UnivMap.t =
    let rec collect (t : t) (m : UnivMap.t) : UnivMap.t =
      match t.desc with 
	| (Assg (x,v) | Test (x,v)) -> UnivMap.add x v m
	| Plus s -> TermSet.fold collect s m
	| Times s -> List.fold_right collect s m
	| (Not x | Star x) -> collect x m
	| (Dup  | Zero  | One ) -> m in
    collect t UnivMap.empty


  let hax_make_star = ref (fun _ -> failwith "hax!") 
  let extract_star_correct pre_extract (pre,e,post) = 
    List.iter2 
      (fun a b -> 
	if compare a b <> 0
	then failwith "extract_star failed !") pre_extract (pre@((!hax_make_star e)::post));
    true

  exception Return of t list * t * t list

  let extract_star tl = 
    try 
      let _ = List.fold_left
	(fun (pre,suff) e -> 
	  match e.desc with 
	    | Star e' -> raise (Return(pre,e',suff))
	    | _ -> e::pre,List.tl suff) 
	([],List.tl tl) tl in 
      failwith "no star to extract here!"
    with Return(pre,e,post) -> 
      let ret = List.rev pre,e,post in 
      if Decide_Util.debug_mode
      then assert(extract_star_correct tl ret);
      ret
      

(* E matrix *)

  let calculate_E d0 =
    let open Decide_Base in 
    let open Base in
    let open Base.Set in
    let negate (x : Decide_Util.Field.t) (v : Decide_Util.Value.t) : Base.Set.t =
      Base.Set.singleton(Base.of_neg_test x v) in
    let get_fixpoint s =
      Printf.printf "getting fixpoint...\n%!";
      let s1 = add (univ_base ()) s in
      (* repeated squaring completes after n steps, where n is the log(cardinality of universe) *)
      let rec f cntr s r =
	if cntr > 1000 then Printf.printf "%u" cntr;
	if equal s r then (Printf.printf "got fixpoint!\n%!"; s)
	else f (cntr + 1) (mult s s) s in
      f 0 (mult s1 s1) s1 in
    let mult_all tl gm = 
      List.fold_right 
	(fun t acc -> mult (gm t) acc) tl
	(singleton (univ_base ())) in 
    
    match d0 with
      | One ->
	(fun _ -> singleton (univ_base ())), (fun _ -> singleton (univ_base ()))
      | Zero ->	(fun _ -> empty),(fun _ -> empty)
      | Assg(field,v) ->
	let r = thunkify (fun _ -> singleton (of_assg field v)) in
	r,r
      | Test(field,v) ->
	let r = thunkify (fun _ -> singleton (of_test field v)) in
	r,r
      | Dup  -> (fun _ -> empty),(fun _ -> singleton (univ_base ()))
      | Plus ts ->
	let r = thunkify (fun _ -> TermSet.fold
	  (fun t acc -> union (t.e_matrix ()) acc) ts empty ) in
	let r_onedup = thunkify (fun _ -> TermSet.fold
	  (fun t acc -> union (t.one_dup_e_matrix ()) acc) ts empty) in
	r,r_onedup
      (* The aE*b unfolding case *)
      | Times tl when has_star tl -> 
	let get_fixpoint_star = get_fixpoint in 
	let get_fixpoint a e = 
	  let rec f a_e sum = 
	    let a_e' = mult a_e e in 
	    let sum' = union a_e' sum in 
	    if equal sum sum' 
	    then sum
	    else f a_e' sum' in 
	  f a empty in 
	let assemble_term gm = 
	  let (pre,e,post) = extract_star tl in 
	  let pre_e = mult_all pre gm in 
	  let post_e = mult_all post gm in 
	  let e = gm e in 
	  List.fold_left union empty
	    [mult pre_e post_e;
	     mult pre_e (mult e post_e);
	     let res = (mult (mult (get_fixpoint pre_e e) e) post_e) in 
	     if Decide_Util.debug_mode
	     then assert (Printf.printf "calling from assert: "; 
			  equal res (mult pre_e (mult e (mult (get_fixpoint_star e) (mult e post_e)))));
	     res] in 
	let me = thunkify (fun _ -> assemble_term (fun x -> x.e_matrix())) in 
	let mo = thunkify (fun _ -> assemble_term (fun x -> x.one_dup_e_matrix ())) in 
	me,mo 
      | Times tl ->
	let r = thunkify (fun _ -> mult_all tl (fun x -> x.e_matrix ())) in
	let r_onedup = thunkify (fun _ -> mult_all tl (fun x -> x.one_dup_e_matrix ())) in 
	r,r_onedup
      | Not x ->
	let m = thunkify (fun _ -> match x.desc with
	  | Zero  -> singleton (univ_base ())
	  | One  -> empty
	  | Test (x,v) -> negate x v
	  | _ -> failwith "De Morgan law should have been applied") in
	m,m
      | Star x ->
	let me = thunkify (fun _ -> get_fixpoint (x.e_matrix())) in
	let mo = thunkify (fun _ -> get_fixpoint (x.one_dup_e_matrix())) in
	me,mo

  let e_matrix t = t.e_matrix () 

  let one_dup_e_matrix t = t.one_dup_e_matrix ()
  

  (* Constructors *)
  let uid_cell = ref 0
  let next_uid () = 
    incr uid_cell;
    !uid_cell

  module FVHash = Hashtbl.Make(struct 
    type t = Field.t * Value.t
    let equal = (=)
    let hash = Hashtbl.hash 
  end)
  module THash = Hashtbl.Make(struct 
    type t = this_t
    let equal = equal
    let hash = hash 
  end)
  module TListHash = Hashtbl.Make(struct 
    type t = this_t list
    let rec equal l1 l2 = 
      match l1,l2 with 
        | [],[] -> true
        | h1::t1, h2::t2 -> 
          compare h1 h2 = 0 && equal t1 t2 
        | _ -> false
    let hash e = Hashtbl.hash (List.map (fun e' -> e'.uid) e)
  end)
  module TSetHash = Hashtbl.Make(struct 
    type t = TermSet.t
    let rec equal s1 s2 = TermSet.equal s1 s2
    let hash = TermSet.hash 
  end)

  let zero_cell = ref None
  let make_zero () = 
    match !zero_cell with 
      | Some t -> t
      | None -> 
        let u = next_uid () in 
        let d = Zero in 
        let h = Hashtbl.hash u in 
	let em,odem = calculate_E d in 
        let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None;
	    e_matrix = em;
	    one_dup_e_matrix = odem} in 
        zero_cell := Some t;
        t

  let one_cell = ref None
  let make_one () = 
    match !one_cell with 
      | Some t -> t
      | None -> 
        let u = next_uid () in 
        let d = One in 
        let h = Hashtbl.hash u in 
	let em,odem = calculate_E d in 
        let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None;
	    e_matrix = em;
	    one_dup_e_matrix = odem} in 
        one_cell := Some t;
        t

  let assg_hash = FVHash.create 101
  let make_assg (f, v) = 
    try FVHash.find assg_hash (f,v)
    with Not_found -> 
      let u = next_uid () in 
      let d = Assg(f,v) in 
      let h = Hashtbl.hash u in 
      let em,odem = calculate_E d in 
      let t = 
        { uid = u;
          desc = d;
          hash = h;
	  spines = None;
	  e_matrix = em;
	  one_dup_e_matrix = odem} in 
      FVHash.add assg_hash (f,v) t;
      t

  let test_hash = FVHash.create 101
  let make_test (f, v) = 
    try FVHash.find test_hash (f,v)
    with Not_found -> 
      let u = next_uid () in 
      let d = Test(f,v) in 
      let h = Hashtbl.hash u in 
      let em,odem = calculate_E d in 
      let t = 
        { uid = u;
          desc = d;
          hash = h;
	  spines = None;
	  e_matrix = em;
	  one_dup_e_matrix = odem} in 
      FVHash.add test_hash (f,v) t;
      t
                
  let dup_cell = ref None
  let make_dup () = 
    match !dup_cell with 
      | Some t -> t
      | None -> 
        let u = next_uid () in 
        let d = Dup in 
        let h = Hashtbl.hash u in 
	let em,odem = calculate_E d in 
        let t = 
          { uid = u;
            desc = d;
            hash = h; 
	    spines = None;
	    e_matrix = em;
	    one_dup_e_matrix = odem} in 
        dup_cell := Some t;
        t


  let plus_hash = TSetHash.create 101
  (* flatten terms *)
  let rec flatten_sum (t : TermSet.t ) : Term.t =
    let f (x : Term.t) = 
      match x.desc with 
	| Term.Plus v -> v
	| Term.Zero -> TermSet.empty
	| _ -> TermSet.singleton x in
    let (t2 : TermSet.t) = TermSet.fold (fun e -> TermSet.union (f e)) t TermSet.empty in 
    match TermSet.elements t2 with 
      | [] -> make_zero ()
      | [x] -> x
      | _ ->  make_plus ~flatten:false t2
  and make_plus ?(flatten = true) (ts : TermSet.t) = 
    try TSetHash.find plus_hash ts 
    with Not_found -> 
      if flatten 
      then flatten_sum ts 
      else 
      let u = next_uid () in 
      let d = Plus(ts) in 
      let h = Hashtbl.hash u in 
      let em,odem = calculate_E d in 
      let t = 
        { uid = u;
          desc = d;
          hash = h;
	  spines = None;
	  e_matrix = em;
	  one_dup_e_matrix = odem} in 
      TSetHash.add plus_hash ts t;
      t

  let times_hash = TListHash.create 101
    
    
  let rec flatten_product (t : Term.t list) : Term.t =
    let f x = match x.desc with 
      | Times v -> v 
      | One  -> [] 
      | _ -> [x] in
    let t1 = List.concat (List.map f t) in
    if List.exists 
      (fun x -> match x.desc with (Zero )  -> true | _ -> false) t1 
    then make_zero ()
    else match t1 with 
      | [] -> make_one ()
      | [x] -> x 
      | _ ->  make_times ~flatten:false t1
  and make_times ?flatten:(flatten=true) ts = 
    try TListHash.find times_hash ts 
    with Not_found -> 
      if flatten
      then flatten_product ts
      else 
	let u = next_uid () in 
	let d = Times(ts) in 
	let h = Hashtbl.hash u in 
	let em,odem = calculate_E d in 
	let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None;
	    e_matrix = em;
	    one_dup_e_matrix = odem} in 
	TListHash.add times_hash ts t;
	t

  let star_hash = THash.create 101 

  let rec flatten_star (t : Term.t) : Term.t =
    let t1 = match t.desc with
      | Term.Plus x -> 
	flatten_sum (TermSet.filter (fun s -> not (is_test s)) x)
      | _ -> t in
    if is_test t1 then make_one ()
    else match t1.desc with
      | Star _ -> t1
      | _ -> make_star ~flatten:false t1
  and make_star ?flatten:(flatten=true) t0 = 
    try THash.find star_hash t0
    with Not_found -> 
      if flatten
      then flatten_star t0 
      else 
	let u = next_uid () in 
	let d = Star(t0) in 
	let h = Hashtbl.hash u in 
	let em,odem = calculate_E d in 
	let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None;
	    e_matrix = em;
	    one_dup_e_matrix = odem} in 
	THash.add star_hash t0 t;
	t

  
  let not_hash = THash.create 101 
(* apply De Morgan laws to push negations down to the leaves *)
  let rec deMorgan (t : Term.t) : Term.t =
    let rec dM (t : Term.t) : Term.t =
      let f x = dM (make_not ~flatten:false x) in
      match t.desc with 
	| (Assg _ | Test _ | Zero  | One  | Dup ) -> t
	| Star (x) -> make_star (dM x)
	| Plus (x) -> make_plus (TermSet.map dM x)
	| Times (x) -> make_times (List.map dM x)
	| Not {desc = Not x} -> dM x
	| Not {desc = Plus s} -> make_times (List.map f (TermSet.elements s))
	| Not {desc = Times s} -> make_plus (TermSet.of_list (List.map f s))
	| Not {desc = Star x} ->
	  if is_test x then make_zero ()
	  else failwith "May not negate an action"
	| Not {desc = Zero} -> make_one ()
	| Not {desc = One} -> make_zero ()
	| Not {desc = Dup} -> failwith "you may not negate a dup!"
	| Not {desc = Assg _} -> failwith "you may not negate an assg!"
	| Not {desc = Test _} -> t in
    dM t
  and flatten_not (t : Term.t) : Term.t =
    match t.desc with
      | Not y -> y
      | Zero -> make_one ()
      | One ->  make_zero ()
      | _ -> make_not ~flatten:false t
  and make_not ?flatten:(flatten=true) t0 = 
    try THash.find not_hash t0
    with Not_found -> 
      if flatten 
      then deMorgan (flatten_not t0)
      else
	let u = next_uid () in 
	let d = Not(t0) in 
	let h = Hashtbl.hash u in 
	let em,odem = calculate_E d in 
	let t = 
          { uid = u;
            desc = d;
            hash = h;
	    spines = None;
	    e_matrix = em;
	    one_dup_e_matrix = odem} in 
	THash.add not_hash t0 t;
	t


  let rec to_string (t : t) : string =
    let out_precedence (t : t) : int =
      match t.desc with
        | Plus _ -> 0
        | Times _ -> 1
        | Not _ -> 2
        | Star _ -> 3
        | _ -> 4 in
    let protect (u:t) : string =
      let s = to_string u in
      if out_precedence t <= out_precedence u then s
      else Printf.sprintf "(%s)" s in 
    let assoc_to_string (op : string) (init : string) (s : string list) : string = 
      match s with
        | [] -> init
        | _ -> String.concat op s in
    match t.desc with
      | Assg (f,v) -> 
        Printf.sprintf "%s:=%s" 
          (Field.to_string f) (Value.to_string v)
      | Test (f,v) -> 
        Printf.sprintf "%s=%s" 
          (Field.to_string f) (Value.to_string v)
      | Dup -> 
        "dup"
      | Plus (ts) -> 
        assoc_to_string " + " "0" 
          (List.map protect (TermSet.elements ts))
      | Times (ts) -> 
        assoc_to_string ";" "1" (List.map protect ts)
      | Not (t) -> 
        (if !utf8 then "¬" else "~") ^ (protect t)
      | Star (t) -> 
        (protect t) ^ "*"
      | Zero -> 
        "drop"
      | One -> 
        "id"

  let make_plus = make_plus ~flatten:true
  let make_times = make_times ~flatten:true
  let make_star = make_star ~flatten:true
  let _ = hax_make_star := make_star
  let make_not = make_not ~flatten:true

  (* Operations *)
  let rspines (t0 : Term.t) : TermSet.t =
    let rec sp (t0 : Term.t) : TermSet.t =
      match t0.desc with
	| Dup -> 
          TermSet.singleton (make_one ())
	| Plus (ts) -> 
          TermSet.bind ts sp
	| Times ([]) -> TermSet.empty
        | Times ([t]) -> sp t
        | Times (th::tt) -> 
          let u = sp th in
          let v = sp (make_times tt) in
          let s = TermSet.map (fun x -> make_times (x :: tt)) u in
          TermSet.union s v
	| Star (t) ->
          let s = sp t in
          TermSet.map (fun x -> make_times [x; t0]) s
	| (Assg _ | Test _ | Not _ | Zero | One) -> 
          TermSet.empty in
    sp t0
      
  let rec lrspines (t0 : Term.t) : TermPairSet.t =
    match t0.spines with 
      | None -> begin 
	  let spines = match t0.desc with
	    | Dup -> 
              TermPairSet.singleton (make_one (), make_one ())
	    | Times ([]) -> 
              TermPairSet.empty
	    | Times([t]) -> 
              lrspines t
	    | Times(th::tt) -> 
	      let u = lrspines th in
	      let v = lrspines (make_times tt) in
	      let f (l,r) = (l, make_times (r :: tt)) in
	      let r = TermPairSet.map f u in
	      let g (l,r) = (make_times [th;l],r) in
	      let s = TermPairSet.map g v in
              TermPairSet.union r s
	    | Star (t) ->
              let s = lrspines t in
              let f (l,r) = (make_times [t0;l], make_times [r;t0]) in 
              TermPairSet.map f s
	    | Plus (ts) -> 
              TermSet.fold (fun x t -> TermPairSet.union (lrspines x) t) ts TermPairSet.empty
	    | (Assg _ | Test _ | Not _ | Zero | One) -> 
              TermPairSet.empty in 
	  t0.spines <- Some spines; 
	  spines end
      | Some spines -> spines
	

end and TermMap : sig 
  include Map.S with type key = Term.t
end  = Map.Make(struct 
  type t = Term.t
  let compare = Term.compare
end
)

and TermPairSet : sig 
  include Set.S with type elt = Term.t * Term.t
  val map : (elt -> elt) -> t -> t
  val bind : t -> (elt -> t) -> t
end = struct 
  include Set.Make(struct 
    type t = Term.t * Term.t
    let compare (al,ar) (bl,br) = 
      match Term.compare al bl with 
	| 0 -> Term.compare ar br
	| o -> o 
  end)
  let map f ts = 
    fold (fun (l,r) acc -> add (f (l,r)) acc) ts empty
  let bind ts f = 
    fold (fun (l,r) t -> union (f (l,r)) t) ts empty

end and TermSet : sig 
  include Set.S with type elt = Term.t
  val map : (elt -> elt) -> t -> t
  val bind : t -> (elt -> t) -> t
  val hash : t -> int
  val to_string : t -> string
  val of_list : Term.t list -> t
end = struct
  include Set.Make(struct
    type t = Term.t

    let compare (ts1:t) (ts2:t) : int = 
      Term.compare ts1 ts2
  end)

  let map (f : elt -> elt) (ts : t) : t =
    fold (fun x -> add (f x)) ts empty

  let bind (ts : t) (f : elt -> t) : t =
    fold (fun x t -> union (f x) t) ts empty

  let hash (ts:t) : int = 
    let open Term in 
    Hashtbl.hash (fold (fun e acc -> e.uid::acc) ts [])

  let to_string (ts:t) : string = 
    fold 
      (fun t acc -> 
        Printf.sprintf "%s%s%s" 
          acc (if acc = "" then "" else "\n")
          (Term.to_string t))
      ts "" 

  let of_list (ts:elt list) : t = 
    List.fold_left (fun acc t -> add t acc) empty ts 
end 


module Formula = struct
  type t =
    | Eq of Term.t * Term.t
    | Le of Term.t * Term.t
        
  let make_eq (t1:Term.t) (t2:Term.t) : t =
    Eq (t1,t2)

  let make_le (t1:Term.t) (t2:Term.t) : t =
    Le (t1,t2)

  let to_string (f:t) : string =
    match f with
      | Eq (s,t) ->
        Printf.sprintf "%s == %s"
          (Term.to_string s) (Term.to_string t)
      | Le (s,t) ->
        Printf.sprintf "%s <= %s"
          (Term.to_string s) (Term.to_string t)

  let compare (f1:t) (f2:t) : int =
    match f1,f2 with
      | Eq(s1,t1), Eq(s2,t2) ->
        let cmp = Term.compare s1 s2 in
        if cmp <> 0 then cmp
        else Term.compare t1 t2
      | Le(s1,t1), Le(s2,t2) ->
        let cmp = Term.compare s1 s2 in
        if cmp <> 0 then cmp
        else Term.compare t1 t2
      | Eq _, _ -> -1
      | _ -> 1

  let equal (f1:t) (f2:t) : bool =
    compare f1 f2 = 0

  let terms (f:t) =
    match f with
      | Eq (s,t) -> (s,t)
      | Le (s,t) -> (s,t)
end


let memoize (f : Term.t -> 'b) : (Term.t -> 'b) = 
  let open Term in 
  let hash = Hashtbl.create 100 in 
  (fun b -> 
    try Hashtbl.find hash b.uid
    with Not_found -> 
      (let ret = f b in 
       Hashtbl.replace hash b.uid ret;
       ret
      )) 
