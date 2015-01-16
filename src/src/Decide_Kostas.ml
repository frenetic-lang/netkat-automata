open Core.Std
open Sexplib.Conv
open Decide_Util
open Tdk

module FieldMap = Map.Make(Field)

type packet = Value.t FieldMap.t with sexp,compare
type point = packet * packet with sexp, compare

module PointSet = Set.Make (struct
    type t = point with sexp, compare
  end)

module PacketSet = Set.Make (struct
    type t = packet with sexp, compare
  end)

exception Empty

module rec TermBase : sig
  type t = term HashCons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSetBase.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One with compare, sexp
end = struct
  type t = term HashCons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup 
    | Plus of TermSetBase.t
    | Times of t list 
    | Not of t
    | Star of t
    | Zero 
    | One with compare, sexp

end and TermSetBase : sig
  include Set.S with type Elt.t = TermBase.t
end = Set.Make (struct
      type t = TermBase.t with compare, sexp
    end)

module Term (* : sig *)
(*   type t = TermBase.t with sexp, compare *)
(*   type term = TermBase.term with sexp, compare *)
(*   val compare_ab : t -> point -> bool *)
(*   val eval : t -> packet -> PacketSet.t *)
(*   val to_string : t -> string *)
(*   val assg : Field.t -> Value.t -> t *)
(*   val test : Field.t -> Value.t -> t *)
(*   val dup : t *)
(*   val plus : TermSet.t -> t *)
(*   val times : t list -> t     *)
(*   val not : t -> t *)
(*   val star : t -> t *)
(*   val zero : t *)
(*   val one : t *)
(* end *) = struct
  include TermBase
  open HashCons
  let rec eval (t : TermBase.t) (pkt : packet) = match t.node with
    | Assg (f,v) -> PacketSet.singleton (FieldMap.add pkt ~key:f ~data:v)
    | Test (f,v) -> begin match FieldMap.find pkt f with
        | Some v' -> if v' = v
          then PacketSet.singleton pkt
          else PacketSet.empty
        | None -> PacketSet.empty
      end
    | Dup -> raise (Failure "t must be dup-free")  
    | Plus ts -> TermSetBase.fold ts ~f:(fun acc t -> PacketSet.union (eval t pkt) acc) ~init:PacketSet.empty
    | Times ts -> List.fold ts ~init:(PacketSet.singleton pkt) ~f:(fun accum t ->
        PacketSet.fold accum ~init:PacketSet.empty ~f:(fun acc pkt -> PacketSet.union acc (eval t pkt)))
    | Not t -> let ret = eval t pkt in
      begin
        match PacketSet.length ret with
        | 0 -> PacketSet.singleton pkt
        | 1 -> PacketSet.empty
        | _ -> raise (Failure "Negation of a non-predicate")
      end
    (* TODO: Copy fixpoint code from Frenetic *)
    | Star t -> raise (Failure "NYI")
    | Zero -> PacketSet.empty
    | One -> PacketSet.singleton pkt

  let compare_ab t point =
    let input,output = point in
    PacketSet.exists (eval t input) ~f:(FieldMap.equal Value.equal output)
      
  let rec to_string (t : t) : string =
    let out_precedence (t : t) : int =
      match t.node with
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
        | _ -> String.concat ~sep:op s in
    match t.node with
      | Assg (f,v) -> 
        Printf.sprintf "%s:=%s" 
          (Field.to_string f) (Value.to_string v)
      | Test (f,v) -> 
        Printf.sprintf "%s=%s" 
          (Field.to_string f) (Value.to_string v)
      | Dup -> 
        "dup"
      | Plus (ts) -> 
        assoc_to_string " + " "drop" 
          (List.map ~f:protect (TermSetBase.elements ts))
      | Times (ts) -> 
        assoc_to_string ";" "id" (List.map ~f:protect ts)
      | Not (t) -> 
        "~" ^ (protect t)
      | Star (t) -> 
        (protect t) ^ "*"
      | Zero -> 
        "drop"
      | One -> 
        "id"

  module H = Make(struct
      type t = TermBase.term with sexp, compare
      let equal a b = compare a b = 0
      let hash = Hashtbl.hash
    end)

  let hashtbl = H.create 100
  let assg f v = H.hashcons hashtbl (Assg (f,v))
  let test f v = H.hashcons hashtbl (Test (f,v))
  let dup = H.hashcons hashtbl Dup
  let plus ts = H.hashcons hashtbl (Plus ts)
  let times ts = H.hashcons hashtbl (Times (List.fold_right ts ~init:[] ~f:(fun x acc -> match x.node with
      | One -> acc
      | Times ts' -> ts' @ acc
      | _ -> x :: acc)))
  let not t = H.hashcons hashtbl (Not t)
  let star t = H.hashcons hashtbl (Star t)
  let zero = H.hashcons hashtbl Zero
  let one = H.hashcons hashtbl One

  module UnivMap = SetMapF(Field)(Value)
  (* Collect the possible values of each variable *)
  let values (t : TermBase.t) : UnivMap.t =
    let rec collect (m : UnivMap.t) (t : TermBase.t) : UnivMap.t =
      match t.node with
	| (Assg (x,v) | Test (x,v)) -> UnivMap.add x v m
	| Plus s -> TermSetBase.fold s ~init:m ~f:collect
	| Times s -> List.fold_right s ~init:m ~f:(fun a b -> collect b a)
	| (Not x | Star x) -> collect m x
	| (Dup  | Zero  | One ) -> m in
    collect UnivMap.empty t

  let equal t1 t2 = compare t1 t2 = 0
end

module TermSet = struct
  include TermSetBase
  let to_string ts = Printf.sprintf "{%s}" (String.concat ~sep:", " (List.map (elements ts) Term.to_string))
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

module type DerivTerm = sig
  type t with sexp

  module EMatrix : sig
    type t with sexp
    val fold : t -> init:'a -> f:('a -> point -> 'a) -> 'a      
    val run : t -> point -> bool
    val compare : t -> t -> int
    val empty : t
    val intersection_empty : t -> t -> bool
    val union : t -> t -> t
  end

  module DMatrix : sig
    type t with sexp
    val run : t -> point -> TermSet.t
    val compare : t -> t -> int
    val equivalent : (TermSet.t -> TermSet.t -> bool) -> t -> t -> bool
    val points : t -> EMatrix.t
  end
  
  val make_term : TermSet.t -> t
  val get_termset : t -> TermSet.t
  (* val to_term : t -> Decide_Ast.Term.t *)
  val get_e : t -> EMatrix.t
  val get_d : t -> DMatrix.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val compare : t -> t -> int
  val to_string : t -> string
end

module rec BDDDeriv : DerivTerm = struct

  open S

  module PartialPacketSet = struct
    include PacketSet
    (* type t = PacketSet.t with sexp *)
    let pktHash pkt = FieldMap.fold pkt ~init:0 ~f:(fun ~key:k ~data:v acc -> acc * 17 + (Field.hash k) * 13 + (Value.hash v) * 11)
    let hash pkts = PacketSet.fold pkts ~init:0 ~f:(fun acc x -> acc * 13 + pktHash x)
    (* pkt <= pkt' *)
    let partialPacketCompare pkt1 pkt2 = try (FieldMap.fold pkt1 ~init:true ~f:(fun ~key:k ~data:v acc ->
        acc && FieldMap.find_exn pkt2 k = v))
      with Not_found -> false
        
    let packetJoin pkt1 pkt2 = try Some (FieldMap.merge pkt1 pkt2 ~f:(fun ~key:k v ->
        match v with
        | `Right v -> Some v
        | `Left v -> Some v
        | `Both (_,v) -> raise Not_found))
      with Not_found -> None
      

    (* let compare p1 p2 = *)
    (*   let pointwiseCompare p p' = PacketSet.for_all p (fun pkt -> PacketSet.exists p' (partialPacketCompare pkt)) in *)
    (*   match pointwiseCompare p1 p2 with *)
    (*   | true -> begin match pointwiseCompare p2 p1 with *)
    (*       | true -> 0 *)
    (*       | false -> -1 *)
    (*     end *)
    (*   | false -> begin match pointwiseCompare p2 p1 with *)
    (*       | true -> 1 *)
    (*       | false -> -1 *)
    (*     end *)
    let compare = PacketSet.compare
    let contains p pkt = PacketSet.exists p (partialPacketCompare pkt)
    (* We could simplify (i.e. {*} U {pkt} -> {*}), but KISS for now *)
    let sum = PacketSet.union
    let prod p1 p2 = PacketSet.fold p1 ~init:PacketSet.empty ~f:(fun acc x ->
        PacketSet.union acc (PacketSet.filter_map p2 (packetJoin x)))
    let zero = PacketSet.empty
    let one = PacketSet.singleton FieldMap.empty
    let union = sum

    let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)
  end

  module PacketDD = Tdk.Vcr.Make(Field)(Value)(PartialPacketSet)

  module EMatrix = struct
    open Term
    open TermBase
    open HashCons

    type t = PacketDD.t

    let cond v t f =
      if PacketDD.equal t f then
        t
      else
        PacketDD.(sum (prod (atom v PartialPacketSet.one PartialPacketSet.zero) t)
                    (prod (atom v PartialPacketSet.zero PartialPacketSet.one) f))

    let mask (t : t) (f,v) = PacketDD.fold (fun r -> PacketDD.const (PartialPacketSet.map r (fun pkt -> match FieldMap.find pkt f with
        | Some v' -> if v = v' then FieldMap.remove pkt f else pkt
        | None -> pkt)))
        (fun v t f -> cond v t f) t
    (* Because (h=v;h<-v) == h=v, we don't have canonical
       representation in DD's. This function canonicalizes by removing
       shadowed modifications 
    *)
    let reduce t = PacketDD.fold (fun r -> PacketDD.const r)
        (fun v t f -> cond v (mask t v) f) t

    let rec t_of_sexp sexp = let open Sexplib in
      match sexp with
      | Sexp.List ss -> begin match List.length ss with
          | 3 -> cond (pair_of_sexp Field.t_of_sexp Value.t_of_sexp (List.nth_exn ss 0))
                   (t_of_sexp (List.nth_exn ss 1))
                   (t_of_sexp (List.nth_exn ss 2))
          | 2 -> PacketDD.const (PartialPacketSet.t_of_sexp (List.nth_exn ss 1))
          | _ -> of_sexp_error "Neither a leaf nor a branch node" sexp
        end
      | Sexp.Atom _ -> failwith "This can't happen"

    let sexp_of_t = let open Sexplib in
      PacketDD.fold (fun r -> Sexp.List [Sexp.Atom "leaf"; PartialPacketSet.sexp_of_t r])
        (fun v t f ->
           Sexp.List [sexp_of_pair Field.sexp_of_t Value.sexp_of_t v;
                      t;
                      f])

    let run t (pkt1,pkt2) =
      match PacketDD.peek (PacketDD.restrict (FieldMap.to_alist pkt1) t) with
      | Some p -> PartialPacketSet.contains p pkt2
      | None -> failwith "Decide_Kostas.BDDDeriv.EMatrix.run failed to get a value from the DD on the pkt"
                     
    let empty = PacketDD.const PartialPacketSet.zero

    let one = PacketDD.const PartialPacketSet.one
    let zero = PacketDD.const PartialPacketSet.zero

    let seq_pkt pkt1 pkt2 = FieldMap.merge pkt1 pkt2 ~f:(fun ~key:k v ->
        match v with
        | `Right v -> Some v
        | `Left v -> Some v
        | `Both (_,v) -> Some v)
    
    let times e1 e2 =
      reduce (PacketDD.fold
                (fun par ->
                   PartialPacketSet.fold par ~init:zero ~f:(fun acc pkt ->
                       let e2' = PacketDD.restrict FieldMap.(to_alist pkt) e2 in
                       PacketDD.(sum (PacketDD.map_r (fun pkts -> PartialPacketSet.map pkts (seq_pkt pkt)) e2') acc)))
                (fun v t f -> cond v t f)
                e1)

    let plus e1 e2 = reduce (PacketDD.sum e1 e2)

    let star e =
      let rec loop acc =
        let acc' = plus one (times e acc) in
        if PacketDD.equal acc acc'
        then acc
        else loop acc'
      in
      reduce (loop e)

    let rec matrix_of_term t =
      let result = match t.node with
        | Plus ts -> TermSet.fold ts ~init:zero ~f:(fun acc v -> plus acc (matrix_of_term v))
        | Dup -> empty
        | Times ts -> List.fold ts ~init:one ~f:(fun acc x -> times acc (matrix_of_term x))
        | Star t -> star (matrix_of_term t)
        | Assg (f,v) -> PacketDD.const (PartialPacketSet.singleton (FieldMap.add FieldMap.empty ~key:f ~data:v))
        | Test (f,v) -> PacketDD.atom (f, v) PartialPacketSet.one PartialPacketSet.zero
        (* Because t should *always* be a predicate, the leaves should only contain either an empty set, or {*} *)
        | Not t -> PacketDD.map_r (fun p -> match PartialPacketSet.equal PartialPacketSet.one p with
            | true -> PartialPacketSet.zero
            | false -> assert (PartialPacketSet.is_empty p); PartialPacketSet.one) (matrix_of_term t)
        | Zero -> zero
        | One -> one in
      reduce result

    let compare t1 t2 = if PacketDD.equal t1 t2 then 0 else -1
    let intersection_empty e e' = PacketDD.equal (PacketDD.prod e e') empty
    let union e1 e2 = reduce (PacketDD.sum e1 e2)
    let intersection e1 e2 = reduce (PacketDD.prod e1 e2)

    let get_points t : PointSet.t =
      let base_points =
        PacketDD.fold
          (fun r -> PartialPacketSet.fold r ~f:(fun acc pkt -> PointSet.add acc (FieldMap.empty, pkt)) ~init:PointSet.empty)
          (fun (h,v) t f ->
             PointSet.union f (PointSet.map t (fun (pkt1,pkt2) -> FieldMap.add pkt1 ~key:h ~data:v, pkt2)))
          t
      in
      Printf.printf "Base points: %s\n" (Sexp.to_string (PointSet.sexp_of_t base_points));
      PointSet.fold base_points ~f:(fun acc pt -> PointSet.union acc (Decide_Util.FieldSet.fold (fun field pts ->
          PointSet.fold pts ~f:(fun acc (a,b) -> PointSet.union acc
          begin
            match FieldMap.find a field, FieldMap.find b field with
            | Some _, Some _ -> PointSet.singleton (a,b)
            | Some x, None -> PointSet.singleton (a, FieldMap.add b ~key:field ~data:x)
            | None, Some _ -> Decide_Util.ValueSet.fold (fun v acc -> PointSet.add acc (FieldMap.add a ~key:field ~data:v, b)) (!Decide_Util.all_values () field) PointSet.empty
            | None, None -> Decide_Util.ValueSet.fold (fun v acc -> PointSet.add acc (FieldMap.add a ~key:field ~data:v, FieldMap.add b ~key:field ~data:v))
                              (!Decide_Util.all_values () field) PointSet.empty
          end) ~init:PointSet.empty) (!Decide_Util.all_fields ()) (PointSet.singleton pt))) ~init:PointSet.empty

    let fold t ~init:init ~f:f =
      let pts = get_points t in
      Printf.printf "get_points: %s\n" (Sexp.to_string (PointSet.sexp_of_t pts));
      PointSet.fold pts ~f:f ~init:init

    let to_string = PacketDD.to_string

    let packet_to_beta pkt = Term.times (FieldMap.fold pkt ~init:[] ~f:(fun ~key:k ~data:v acc ->
        assg k v :: acc))
        
    let betas = PacketDD.fold (fun pkts -> PartialPacketSet.fold pkts ~init:TermSet.empty ~f:(fun acc x -> TermSet.union acc (TermSet.singleton (packet_to_beta x)))) (fun _ -> TermSet.union)

  end

  module DMatrix = struct

    open HashCons

    type compact_derivative = {
      left_hand : EMatrix.t;
      right_hand : Term.t
    } with compare, sexp

    module CompactDerivSet = Set.Make (struct
        type t = compact_derivative with compare, sexp
      end)

    type t = CompactDerivSet.t with sexp, compare

    (* let compare = t_compare *)
        
    let run t point =
      CompactDerivSet.fold t ~init:TermSet.empty
        ~f:(fun acc deriv -> if EMatrix.run deriv.left_hand point
             then TermSet.union (TermSet.map (EMatrix.betas deriv.left_hand) ~f:(fun b -> Term.times [b; deriv.right_hand])) acc
             else acc)

    let term_append t e = Term.times [t; e]
    let left_app e d = { d with left_hand = EMatrix.times e d.left_hand }
    let right_app d e = { d with right_hand = term_append d.right_hand e }

    let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e)
    let d_left_app e ds = CompactDerivSet.map ds (left_app e)


    let matrix_of_term t =
      let rec matrix_of_term' t =
        let open Term in
        begin match t.node with
        | Dup -> CompactDerivSet.singleton ({ left_hand = EMatrix.one; right_hand = one })
        | Plus ts -> TermSet.fold ts ~f:(fun acc t -> CompactDerivSet.union (matrix_of_term' t) acc) ~init:CompactDerivSet.empty
        | Times (t::ts) -> CompactDerivSet.union (d_right_app (matrix_of_term' t) (times ts))
                             (d_left_app (EMatrix.matrix_of_term t) (matrix_of_term' (times ts)))
        | Star t -> d_left_app (EMatrix.matrix_of_term (star t)) (d_right_app (matrix_of_term' t) (star t))
        | _ -> CompactDerivSet.empty
        end in
      TermSet.fold t ~init:CompactDerivSet.empty ~f:(fun acc x -> CompactDerivSet.union acc (matrix_of_term' x))

    (* 
       a) for each (b, e) \in D(elm1), (b',e') \in D(elm2), 
          if b /\ b' != 0, then e bisim e'
       b) \/ b == \/ b'
    *)

    let rec power_set = function
      | [] -> [[]]
      | x :: xs -> let rem = power_set xs in
        rem @ (List.map rem (fun y -> x :: y))

    let d_equivalent bisim d1 d2 =
      let compute_intersections d = 
        let dlst = CompactDerivSet.elements d in
        (* We can be smarter and filter out empty-intersections *)
        List.map (power_set dlst) (fun xs -> List.fold xs ~init:(EMatrix.one, TermSet.empty) ~f:(fun (e,ts) t ->
            let e' = EMatrix.intersection e t.left_hand in
            if EMatrix.compare e' EMatrix.zero = 0 then
              (EMatrix.zero, TermSet.empty)
            else
              (e', TermSet.union ts (TermSet.singleton t.right_hand)))) in
        List.for_all (List.cartesian_product (compute_intersections d1) (compute_intersections d2))
          (fun ((e,d),(e',d')) -> match EMatrix.intersection_empty e e' with
             | true -> bisim d d'
             | false -> true)
          
    let equivalent (bisim : TermSet.t -> TermSet.t -> bool) d1 d2 =
      d_equivalent bisim d1 d2
      && EMatrix.compare (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc))
        (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc)) = 0

    let points t = CompactDerivSet.fold t ~init:EMatrix.zero ~f:(fun acc x -> EMatrix.union acc x.left_hand)

    let to_string t = Printf.sprintf "{%s}" (String.concat ~sep:"; " (List.map (CompactDerivSet.elements t)
                                                                        (fun elm -> Printf.sprintf "(%s,%s)" (EMatrix.to_string elm.left_hand)
                                                                            (Term.to_string elm.right_hand))))
  end

  type t = { desc : TermSet.t;
             mutable e_matrix : EMatrix.t option;
             mutable d_matrix : DMatrix.t option
           } with sexp

  let get_termset t = t.desc

  let get_e t = match t.e_matrix with
    | Some e -> e
    | None -> let e = EMatrix.matrix_of_term (Term.plus t.desc) in
      t.e_matrix <- Some e;
      e

  let get_d t = match t.d_matrix with
    | Some d -> d
    | None -> let d = DMatrix.matrix_of_term t.desc in
      t.d_matrix <- Some d;
      d

  let get_term t = t.desc

  let sexp_of_t t = t.e_matrix <- Some (get_e t);
    t.d_matrix <- Some (get_d t);
    sexp_of_t t

  let make_term terms =
    (* let open HashCons in *)
    (* let terms = match term.node with *)
    (*   | TermBase.Plus ts -> ts *)
    (*   | _ -> TermSet.singleton term in *)
    { desc = terms;
      e_matrix = None;
      d_matrix = None
    }

  let compare t t' = TermSet.compare t.desc t'.desc
  let to_string t = Printf.sprintf "[desc: %s; e_matrix: %s; d_matrix: %s]"
      (TermSet.to_string t.desc)
      (EMatrix.to_string (get_e t))
      (DMatrix.to_string (get_d t))
end
  
(* module rec KostasDeriv : DerivTerm = struct *)
  
(*   module EMatrix = struct *)
(*     type t = Term.t with sexp   *)
(*     let run = Term.compare_ab *)
(*     open HashCons *)
                
(*     let rec matrix_of_term t = *)
(*       let open Term in *)
(*       let open TermBase in *)
(*       match t.node with *)
(*       | Plus ts -> plus (TermSet.map ts matrix_of_term) *)
(*       | Dup -> zero *)
(*       | Times ts -> times (List.map ts matrix_of_term) *)
(*       | Star t -> star (matrix_of_term t) *)
(*       | _ -> t *)
        
(*     let compare _ _ = failwith "NYI: Decide_Kostas.DerivTerm.EMatrix.compare" *)
(*     let intersection_empty e e' = failwith "NYI: Decide_Kostas.EMatrix.intersection" *)
(*     let empty = Term.zero *)
(*     let union e1 e2 = Term.plus (TermSet.of_list [e1;e2]) *)
(*   end *)

(*   module DMatrix = struct *)

(*     open HashCons *)

(*     type compact_derivative = { *)
(*       left_hand : EMatrix.t; *)
(*       right_hand : Term.t; *)
(*     } with compare, sexp *)

(*     module CompactDerivSet = Set.Make (struct *)
(*         type t = compact_derivative with compare, sexp *)
(*       end) *)

(*     type t = CompactDerivSet.t with sexp *)

(*     let compare _ _ = failwith "NYI: Decide_Kostas.DerivTerm.EMatrix.compare" *)
        
(*     let run t point = *)
(*       CompactDerivSet.fold t ~init:TermSet.empty *)
(*         ~f:(fun acc deriv -> if Term.compare_ab deriv.left_hand point *)
(*              then TermSet.union (TermSet.singleton deriv.right_hand) acc *)
(*              else acc) *)

(*     let term_append t e = Term.times [t; e] *)
(*     let left_app e d = { d with left_hand = term_append d.left_hand e } *)
(*     let right_app d e = { d with right_hand = term_append d.right_hand e } *)

(*     let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e) *)
(*     let d_left_app e ds = CompactDerivSet.map ds (left_app e) *)


(*     let rec matrix_of_term t = *)
(*       let open Term in *)
(*       match t.node with *)
(*       | Dup -> CompactDerivSet.singleton ({ left_hand = one; right_hand = one }) *)
(*       | Plus ts -> TermSet.fold ts ~f:(fun acc t -> CompactDerivSet.union (matrix_of_term t) acc) ~init:CompactDerivSet.empty *)
(*       | Times (t::ts) -> CompactDerivSet.union (d_right_app (matrix_of_term t) (times ts)) *)
(*                            (d_left_app (EMatrix.matrix_of_term t) (matrix_of_term (times ts))) *)
(*       | Star t -> d_left_app (star t) (d_right_app (matrix_of_term t) (star t)) *)
(*       | _ -> CompactDerivSet.empty *)

(*     (\*  *)
(*        a) for each (b, e) \in D(elm1), (b',e') \in D(elm2),  *)
(*           if b /\ b' != 0, then e bisim e' *)
(*        b) \/ b == \/ b' *)
(*     *\) *)
(*     let equivalent bisim d1 d2 = *)
(*       CompactDerivSet.for_all d1 ~f:(fun elm1 -> CompactDerivSet.for_all d2 ~f:(fun elm2 -> *)
(*           EMatrix.intersection_empty elm1.left_hand elm2.left_hand *)
(*           || bisim elm1.right_hand elm2.right_hand)) *)
(*       && EMatrix.compare (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc)) *)
(*         (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc)) = 0 *)
(*   end *)

(*   type t = { desc : TermSet.t; *)
(*              mutable e_matrix : EMatrix.t option; *)
(*              mutable d_matrix : DMatrix.t option *)
(*            } with sexp *)
    
(*   let get_e t = match t.e_matrix with *)
(*     | Some e -> e *)
(*     | None -> let e = EMatrix.matrix_of_term (Term.plus t.desc) in *)
(*       t.e_matrix <- Some e; *)
(*       e *)

(*   let get_d t = match t.d_matrix with *)
(*     | Some d -> d *)
(*     | None -> let d = DMatrix.matrix_of_term (Term.plus t.desc) in *)
(*       t.d_matrix <- Some d; *)
(*       d *)

(*   let get_term t = Term.plus t.desc *)

(*   let to_string = failwith "NYI: KostasDeriv.to_string" *)
      
(*   let make_term term = *)
(*     { desc = term; *)
(*       e_matrix = None; *)
(*       d_matrix = None *)
(*     } *)

(*   let compare t t' = failwith "NYI: Decide_Kostas.KostasDeriv.compare" *)
(* end *)

