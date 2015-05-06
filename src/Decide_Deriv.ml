open Core.Std
open Sexplib.Conv
open Decide_Util
open Tdk
open Decide_Ast

exception Empty

module PointSet = struct
  include FiniteSet (struct
      type t = point with sexp, compare
      module S = Set.Make (struct
          type t = point with sexp, compare
        end)
      let size () = (PacketSet.length PacketSet.all * PacketSet.length PacketSet.all)
      let universe () = PacketSet.fold PacketSet.all ~init:S.empty
          ~f:(fun acc pkt -> S.union acc (PacketSet.fold PacketSet.all ~init:S.empty
                                            ~f:(fun acc pkt' -> S.add acc (pkt,pkt'))))
    end)
  let to_string pts = Printf.sprintf "{%s}" (String.concat ~sep:"," (fold pts ~init:[] ~f:(fun acc pt -> point_to_string pt :: acc)))
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
    val intersection : t -> t -> t
    val union : t -> t -> t
    val complement : t -> t
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
  val run_exact : t -> point -> TermSet.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val compare : t -> t -> int
  val to_string : t -> string
end

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
      | `Both (v,v') -> if v = v' then Some v else raise Not_found))
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
  let to_string t = Printf.sprintf "{%s}" (String.concat ~sep:", " (PacketSet.fold t ~f:(fun lst pkt -> packet_to_string pkt :: lst) ~init:[]))
end

module PacketDD = Tdk.Vcr.Make(Field)(Value)(PartialPacketSet)

module TermMatrix = functor () -> struct
  open Term
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
  let reduce t = t (* PacketDD.fold (fun r -> PacketDD.const r) *)
  (* (fun v t f -> cond v (mask t v) f) t *)

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

  let seq_pkt pkt1 pkt2 = FieldMap.merge pkt1 pkt2 ~f:(fun ~key:k v ->
      match v with
      | `Right v -> Some v
      | `Left v -> Some v
      | `Both (_,v) -> Some v)

  let run t (pkt1,pkt2) =
    match PacketDD.peek (PacketDD.restrict (FieldMap.to_alist pkt1) t) with
    | Some p -> PartialPacketSet.contains (PartialPacketSet.map p (seq_pkt pkt1)) pkt2
    | None -> failwith "Decide_Deriv.BDDDeriv.EMatrix.run failed to get a value from the DD on the pkt"

  let empty = PacketDD.const PartialPacketSet.zero

  let one = PacketDD.const PartialPacketSet.one
  let zero = PacketDD.const PartialPacketSet.zero
  let all = PacketDD.const PartialPacketSet.all

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

  let get_points t : PointSet.t =
    let base_points =
      PacketDD.fold
        (fun r -> PartialPacketSet.fold r ~f:(fun acc pkt -> PointSet.add acc (FieldMap.empty, pkt)) ~init:PointSet.empty)
        (fun (h,v) t f ->
           (* let extra_pkt = FieldMap.add FieldMap.empty ~key:h ~data:Value.extra_val in *)
           PointSet.union (PointSet.map t (fun (pkt1,pkt2) -> FieldMap.add pkt1 ~key:h ~data:v, pkt2))
             (* Expensive solution. Should be cheaper way to express negative constrains *)
             (PointSet.fold f ~f:(fun acc (pkt1,pkt2) -> PointSet.union acc (Decide_Util.ValueSet.fold
                                 (fun v' acc -> if v <> v'
                                   then
                                     (* Need to be careful and not overwrite fields chosen later down in the tree *)
                                     match FieldMap.find pkt1 h with
                                     | Some _ -> PointSet.add acc (pkt1, pkt2)
                                     | None -> PointSet.add acc (FieldMap.add pkt1 ~key:h ~data:v', pkt2)
                                   else acc) (!Decide_Util.all_values () h) PointSet.empty))
             ~init:PointSet.empty))
        t
    in
    (* Printf.printf "Base points: %s\n" (PointSet.to_string base_points); *)
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
    (* Printf.printf "get_points: %s\n" (PointSet.to_string pts); *)
    PointSet.fold pts ~f:f ~init:init

  (* Since PacketDD is not guaranteed to be canonical, we have to semantically compare *)
  let compare t1 t2 = let eq = PacketDD.equal t1 t2 ||
                               (fold t1 ~init:true ~f:(fun acc pt -> acc &&
                                                                     run t1 pt = run t2 pt)
                                && fold t2 ~init:true ~f:(fun acc pt -> acc && run t1 pt = run t2 pt)) in
    if eq then 0 else -1

  let compare_cheap t1 t2 = if PacketDD.equal t1 t2 then 0 else -1

  let set_intersection e s = PacketDD.map_r (PartialPacketSet.inter s) e

  let restrict (h,v) e = PacketDD.fold (fun x -> PacketDD.const x)
      (fun (h',v') left right -> if h = h then
          match v = v' with
          | true -> left
          | false -> right
        else cond (h', v') left right) e

  let restrict_neg (h,v) e = PacketDD.fold (fun x -> PacketDD.const x)
      (fun (h',v') left right -> if h = h && v = v' then
          right
        else
          cond (h', v') left right) e

  let intersection e1 e2 = PacketDD.fold
      (set_intersection e2)
      (fun (h,v) left right -> cond (h,v) (PacketDD.restrict [(h,v)] left)
          (restrict_neg (h,v) right))
      e1

  (* let intersection e1 e2 = reduce (PacketDD.prod e1 e2) *)

  let to_string = PacketDD.to_string

  (* Note: it's not correct to simply complement the sets at the
     leaves. The leave sets are *not* sets of packets, but sets of
     modifications. The set of resulting packets depends upon the path
     taken to reach that set. Thus, when we complement the sets of
     modifications, we have to complement them w.r.t the path. *)
  let complement t = failwith "NYI: BDDDeriv.EMatrix.complement"
  let assg f v = PacketDD.const (PartialPacketSet.singleton (FieldMap.add FieldMap.empty ~key:f ~data:v))
  let test f v = PacketDD.atom (f, v) PartialPacketSet.one PartialPacketSet.zero
  (* Because t should *always* be a predicate, the leaves should only contain either an empty set, or {*} *)      
  let not t = PacketDD.map_r (fun p -> match PartialPacketSet.equal PartialPacketSet.one p with
      | true -> PartialPacketSet.zero
      | false -> assert (PartialPacketSet.is_empty p); PartialPacketSet.one) t

  let rec matrix_of_term t =
    let result = match t.node with
      | Zero -> zero
      | One -> one
      | All -> all
      | Plus ts -> TermSet.fold ts ~init:zero ~f:(fun acc v -> plus acc (matrix_of_term v))
      | Dup -> empty
      | Times ts -> List.fold ts ~init:one ~f:(fun acc x -> times acc (matrix_of_term x))
      | Star t -> star (matrix_of_term t)
      | Assg (f,v) -> assg f v
      | Test (f,v) -> test f v
      | Not t -> not (matrix_of_term t)
      | Complement t -> complement (matrix_of_term t)
      | Intersection ts -> (* Printf.printf "Folding over intersection: %s\n" (Term.to_string t); *)
        TermSet.fold ts ~init:(matrix_of_term Term.all) ~f:(fun acc x ->
            let mat = matrix_of_term x in
            let res = intersection acc mat in
            (* Printf.printf "acc:          %s\nx:            %s\nintersection: %s\n" *)
            (*   (to_string acc) (to_string mat) (to_string res); *)
            res)
    in
    reduce result

  let union e1 e2 = reduce (PacketDD.sum e1 e2)

  let intersection_empty e e' = (compare (PacketDD.prod e e') empty) = 0   

  let packet_to_beta pkt = Term.times (FieldMap.fold pkt ~init:[] ~f:(fun ~key:k ~data:v acc ->
      Term.test k v :: acc))

  (* TODO: I'm not sure this handles the negative branch correctly. *)
  let betas = PacketDD.fold (fun pkts -> PartialPacketSet.fold pkts ~init:TermSet.empty
                                ~f:(fun acc x -> TermSet.union acc (TermSet.singleton (packet_to_beta x))))
      (fun (h,v) t f -> TermSet.union (TermSet.map t (fun beta -> Term.(times [test h v; beta])))
          (TermSet.map f (fun beta -> Term.(times [not (test h v); beta]))))
end

module rec BDDDeriv : functor () -> DerivTerm = functor () -> struct

  open S

  module EMatrix = TermMatrix ()

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

    let compact_derivative_to_string elm = Printf.sprintf "(%s,%s)" (EMatrix.to_string elm.left_hand)
                                                                            (Term.to_string elm.right_hand)
    let to_string t = Printf.sprintf "{%s}" (String.concat ~sep:"; " (List.map (CompactDerivSet.elements t) compact_derivative_to_string))

    let pkt_to_beta pkt = Term.times (FieldMap.fold pkt ~init:[] ~f:(fun ~key ~data acc -> Term.test key data :: acc))
               
    let run t point =
      CompactDerivSet.fold t ~init:TermSet.empty
        ~f:(fun acc deriv ->
            let result = EMatrix.run deriv.left_hand point in
            (* Printf.printf "Running %s on %s: %b\n" (compact_derivative_to_string deriv) (point_to_string point) result; *)
               if result
             then TermSet.add acc (Term.times [pkt_to_beta (snd point); deriv.right_hand])
             else acc)

    let term_append t e = Term.times [t; e]
    let left_app e d = { d with left_hand = EMatrix.times e d.left_hand }
    let right_app d e = { d with right_hand = term_append d.right_hand e }

    let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e)
    let d_left_app e ds = CompactDerivSet.map ds (left_app e)


    let intersect d1 d2 =
      {left_hand = EMatrix.intersection d1.left_hand d2.left_hand;
       right_hand = Term.intersection (TermSet.of_list [d1.right_hand; d2.right_hand])}

    let matrix_of_term t =
      let rec matrix_of_term' t =
        let open Term in
        begin match t.node with
          | Dup             -> CompactDerivSet.singleton ({ left_hand = EMatrix.one;
                                                            right_hand = one })
          | Plus ts         -> TermSet.fold ts
                               ~f:(fun acc t -> CompactDerivSet.union (matrix_of_term' t) acc)
                               ~init:CompactDerivSet.empty
          | Times (t::ts)   -> CompactDerivSet.union (d_right_app (matrix_of_term' t) (times ts))
                                 (d_left_app (EMatrix.matrix_of_term t)
                                    (matrix_of_term' (times ts)))
          | Star t          -> d_left_app (EMatrix.matrix_of_term (star t))
                                 (d_right_app (matrix_of_term' t) (star t))
          | Intersection ts -> let intersect d1 d2 = CompactDerivSet.fold d1
              ~f:(fun acc x -> CompactDerivSet.union acc
                     (CompactDerivSet.map d2
                        ~f:(fun d ->
                          {left_hand = EMatrix.intersection d.left_hand x.left_hand;
                           right_hand = Term.intersection (TermSet.of_list [d.right_hand;
                                                                            x.right_hand])})))
              ~init:CompactDerivSet.empty in
            TermSet.fold ts ~f:(fun acc t -> intersect acc (matrix_of_term' t))
              ~init:(CompactDerivSet.singleton ({left_hand = EMatrix.matrix_of_term all;
                                                 right_hand = Term.all}))
          | Complement t          -> let intersect d1 d2 = CompactDerivSet.fold d1
              ~f:(fun acc x ->
                  let e = EMatrix.intersection d2.left_hand x.left_hand in
                  let trans = Term.intersection (TermSet.of_list [(complement d2.right_hand);
                                                                  x.right_hand]) in
                  CompactDerivSet.add acc {left_hand = e;
                                           right_hand = trans})
              ~init:CompactDerivSet.empty
            in
            let d = CompactDerivSet.fold (matrix_of_term' t) ~f:intersect
                ~init:(CompactDerivSet.singleton ({left_hand = EMatrix.matrix_of_term all;
                                                   right_hand = Term.all})) in
            let d_pos = CompactDerivSet.fold d ~f:(fun dom d ->
                EMatrix.union dom d.left_hand) ~init:EMatrix.empty in
            CompactDerivSet.add d {left_hand = EMatrix.complement d_pos;
                                   right_hand = Term.all}
          | All -> CompactDerivSet.singleton ({left_hand = EMatrix.matrix_of_term all;
                                               right_hand = Term.all})
          | Not _
          | Times []
          | Assg _
          | Test _
          | One
          | Zero            -> CompactDerivSet.empty
                    
        end in
      TermSet.fold t
        ~init:CompactDerivSet.empty
        ~f:(fun acc x -> CompactDerivSet.union acc (matrix_of_term' x))

    let rec power_set = function
      | [] -> [[]]
      | x :: xs -> let rem = power_set xs in
        rem @ (List.map rem (fun y -> x :: y))


    (* 
       a) for each (b, e) \in D(elm1), (b',e') \in D(elm2), 
          if b /\ b' != 0, then e bisim e'
       b) \/ b == \/ b'
    *)
    
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

  open HashCons
  open Term
  let rec run_e_exact t pkt = match t.node with
    | Assg (h,v) -> PacketSet.singleton (FieldMap.add pkt ~key:h ~data:v)
    | Test (h,v) -> begin match FieldMap.find pkt h with
        | Some v' -> if v' = v then PacketSet.singleton pkt else PacketSet.empty
        | None -> PacketSet.empty
      end
    | Dup -> PacketSet.empty
    | Plus ts -> TermSet.fold ts ~f:(fun acc t -> PacketSet.union acc (run_e_exact t pkt))
                   ~init:PacketSet.empty
    | Times (t::ts) -> PacketSet.fold (run_e_exact t pkt) ~f:(fun acc pkt' ->
        PacketSet.union acc (run_e_exact (times ts) pkt')) ~init:PacketSet.empty
    | Times [] -> PacketSet.singleton pkt
    | Star t -> let rec loop pkts gamma =
      let iter = run_e_exact t gamma in
      let pkts' = PacketSet.union pkts (PacketSet.singleton gamma) in
      if PacketSet.subset iter pkts' then
        pkts'
      else PacketSet.fold (PacketSet.diff iter pkts')
          ~f:loop ~init:pkts' in
      loop PacketSet.empty pkt
    | Intersection ts -> TermSet.fold ts ~f:(fun acc t -> PacketSet.inter acc (run_e_exact t pkt)) ~init:(run_e_exact (TermSet.choose_exn ts) pkt)
    | Complement t -> PacketSet.complement (run_e_exact t pkt)
    | Zero -> PacketSet.empty
    | One -> PacketSet.singleton pkt
    | All -> PacketSet.all
    | Not t -> match PacketSet.length (run_e_exact t pkt) with
      | 0 -> PacketSet.singleton pkt
      | 1 -> PacketSet.empty
      | _ -> failwith "called run_e_exact (Not t) where t was not a predicate!"

  let packet_to_term pkt = times (FieldMap.fold pkt ~init:[]
                                    ~f:(fun ~key ~data t -> (assg key data) :: t))

  let rec run_exact' t pt = match t.node with
    | Assg _
    | Test _ -> TermSet.empty
    | Dup -> if (fst pt) = (snd pt) then TermSet.singleton (packet_to_term (fst pt)) else TermSet.empty
    | Plus ts -> TermSet.fold ts ~f:(fun acc t -> TermSet.union acc (run_exact' t pt)) ~init:TermSet.empty
    (* TODO: Smart way to do this is to "extract" the gammas that make the product non-zero instead of iterating over all possible ones *)
    | Times (t::ts) -> TermSet.union (TermSet.singleton (times (plus (run_exact' t pt) :: ts)))
                         (PacketSet.fold (run_e_exact t (fst pt))
                            ~f:(fun acc pkt -> TermSet.union acc (run_exact' (times ts) (pkt, snd pt))) ~init:TermSet.empty)
    | Times [] -> TermSet.empty
    | Intersection ts -> TermSet.singleton (intersection (TermSet.fold ts ~f:(fun acc t -> TermSet.union acc (run_exact' t pt)) ~init:TermSet.empty))
    | Not t -> TermSet.empty
    | Complement t -> TermSet.singleton (complement (plus (run_exact' t pt)))
    | Star t -> let rec loop ts gamma =
      let iter = TermSet.map (run_exact' t (gamma, snd pt)) ~f:(fun e -> times [e; star t]) in
      if TermSet.subset iter ts
      then
        ts
      else
        PacketSet.fold (run_e_exact t gamma) ~f:loop ~init:(TermSet.union ts iter) in
      (loop TermSet.empty (fst pt))
    | Zero -> TermSet.empty
    | One -> TermSet.empty
    | All -> TermSet.singleton all

  let run_exact t pt = match t.d_matrix with
    | Some d -> DMatrix.run d pt
    | None -> run_exact' (plus t.desc) pt

  let compare t t' = TermSet.compare t.desc t'.desc
  let to_string t = Printf.sprintf "[desc: %s;\n e_matrix: %s;\n d_matrix: %s]"
      (TermSet.to_string t.desc)
      (EMatrix.to_string (get_e t))
      (DMatrix.to_string (get_d t))
end

module BDDMixed () = struct

  module TermMatrix = TermMatrix ()

  module rec MixedTermBase : sig
    type t =
      | FDD of TermMatrix.t
      | Plus of MixedSet.t
      | Times of t list
      | Intersection of MixedSet.t
      | Complement of t
      | Star of t
      | All
      | Zero
      | One with compare, sexp
    val get_points : t -> PointSet.t
    val eval : t -> packet -> PacketSet.t
    val fold : t -> init:'a -> f:('a -> point -> 'a) -> 'a
    val run : t -> point -> bool
    val to_string : t -> string
  end = struct
    type t =
      | FDD of TermMatrix.t
      | Plus of MixedSet.t
      | Times of t list
      | Intersection of MixedSet.t
      | Complement of t
      | Star of t
      | All
      | Zero
      | One with compare, sexp


    let rec eval t pkt = match t with
      | FDD f -> TermMatrix.fold f ~init:PacketSet.empty ~f:(fun acc (a,b) ->
          if FieldMap.equal Value.equal a pkt then PacketSet.add acc pkt else acc)
      | Plus ts -> MixedSet.fold ts ~init:PacketSet.empty
                     ~f:(fun acc t -> PacketSet.union acc (eval t pkt))
      | Times ts -> List.fold ts ~init:(PacketSet.singleton pkt)
                      ~f:(fun pkts t ->
                          PacketSet.fold pkts ~init:PacketSet.empty
                            ~f:(fun acc pkt -> PacketSet.union acc (eval t pkt)))
      | Intersection ts -> MixedSet.fold ts ~init:PacketSet.all
                             ~f:(fun acc t -> PacketSet.inter acc (eval t pkt))
      | Complement t -> PacketSet.complement (eval t pkt)
      | Star t -> let rec loop pkts =
        let iter = PacketSet.fold pkts ~init:pkts ~f:(fun acc pkt ->
            PacketSet.union acc (eval t pkt)) in
        if PacketSet.equal iter pkts
        then pkts
        else loop iter in
        loop (PacketSet.singleton pkt)
      | All -> PacketSet.all
      | Zero -> PacketSet.empty
      | One -> PacketSet.singleton pkt

    let rec get_points t = match t with
      | FDD f -> TermMatrix.get_points f
      | Plus ts -> MixedSet.fold ts ~init:PointSet.empty
                     ~f:(fun acc t -> PointSet.union acc (get_points t))
      | Times (t::ts) -> List.fold ts ~init:(get_points t)
                           ~f:(fun pts t ->
                               PointSet.fold pts ~init:PointSet.empty
                                 ~f:(fun acc (a,b) -> PacketSet.fold (eval t b)
                                        ~init:PointSet.empty ~f:(fun pts pkt -> PointSet.add pts (a,pkt))))
      | Intersection ts -> MixedSet.fold ts ~init:PointSet.all
                             ~f:(fun acc t -> PointSet.inter acc (get_points t))
      | Complement t -> PointSet.complement (get_points t)
      | Star t -> let rec loop pts =
        let iter = PointSet.fold pts ~init:pts ~f:(fun acc (a,b) ->
            PointSet.union acc (PacketSet.fold (eval t b) ~init:PointSet.empty
                                  ~f:(fun acc pkt -> PointSet.add acc (a,pkt)))) in
        if PointSet.equal iter pts
        then pts
        else loop iter in
        loop (PacketSet.fold PacketSet.all ~init:PointSet.empty ~f:(fun acc pkt ->
            PointSet.add acc (pkt,pkt)))
      | All -> PointSet.all
      | Zero -> PointSet.empty
      | One -> PacketSet.fold PacketSet.all ~init:PointSet.empty ~f:(fun acc pkt ->
          PointSet.add acc (pkt,pkt))

    let fold t ~init ~f =
      PointSet.fold (get_points t) ~init ~f

    let rec run t pt = match t with
      | FDD f -> TermMatrix.run f pt
      | Plus ts -> MixedSet.exists ts (fun t -> run t pt)
      | Times ts -> let a,b = pt in
        PacketSet.mem (eval t a) b
      | Intersection ts -> MixedSet.for_all ts (fun t -> run t pt)
      | Complement t -> Pervasives.not (run t pt)
      (* TODO: how should I do star here? *)
      | Star _ -> let a,b = pt in
        PacketSet.mem (eval t a) b
      | All -> true
      | Zero -> false
      | One -> FieldMap.equal Value.equal (fst pt) (snd pt)

    let rec to_string t = match t with
      | FDD f -> Printf.sprintf "FDD %s" (TermMatrix.to_string f)
      | Plus ts -> Printf.sprintf "Plus {%s}"
                     (String.concat
                        (MixedSet.fold ts ~init:[]
                           ~f:(fun acc t -> (to_string t) :: acc)) ~sep:";")
      | Times ts -> Printf.sprintf "Times [%s]"
                      (String.concat
                         (List.fold ts ~init:[]
                            ~f:(fun acc t -> acc @ [to_string t])) ~sep:";")
      | Intersection ts -> Printf.sprintf "Inter {%s}"
                             (String.concat
                                (MixedSet.fold ts ~init:[]
                                   ~f:(fun acc t -> (to_string t) :: acc)) ~sep:";")
      | Complement t -> Printf.sprintf "Comp (%s)" (to_string t)
      | Star t -> Printf.sprintf "Star (%s)" (to_string t)
      | All -> "All"
      | Zero -> "Zero"
      | One -> "One"
  end and MixedSet : sig
  include Set.S with type Elt.t = MixedTermBase.t
  val to_string : t -> string
end = struct
  include Set.Make (struct
      type t = MixedTermBase.t with sexp, compare
    end)
  let to_string ts = Printf.sprintf "{%s}" (String.concat ~sep:"," (fold ts ~init:[] ~f:(fun acc t -> MixedTermBase.to_string t :: acc)))
end

  module MixedTerm = struct
    include MixedTermBase

    let all = All            
    let zero = Zero
    let one = One
    let empty = zero

    let lift_constants t = match t with
      | FDD f -> if TermMatrix.compare_cheap f TermMatrix.zero = 0
        then zero
        else if TermMatrix.compare_cheap f TermMatrix.one = 0
        then one
        else if TermMatrix.compare_cheap f TermMatrix.all = 0
        then all
        else FDD f
      | _ -> t

    let assg f v = FDD (TermMatrix.assg f v)
    let test f v = FDD (TermMatrix.test f v)
    (* TODO: Can probably do de Morgan to push comp up *)
    let pluss ts =
      let plus' ts = let f, ts' = MixedSet.fold ts ~f:(fun (f_acc,set_acc) t -> match t,set_acc with
          | _, None -> TermMatrix.all, None
          | FDD f, set_acc -> TermMatrix.plus f f_acc, set_acc
          | Zero, set_acc -> f_acc, set_acc
          | All,_ -> TermMatrix.all, None
          | _ , Some set_acc -> f_acc, Some (MixedSet.add set_acc t))
          ~init:(TermMatrix.zero, Some MixedSet.empty) in
        match ts' with
        | None -> all
        | Some ts' ->
          if MixedSet.is_empty ts'
          then FDD f
          else if MixedSet.length ts' = 1
          then MixedSet.choose_exn ts'
          else Plus (MixedSet.add ts' (FDD f)) in
      lift_constants (plus' ts)

    let plus t1 t2 = match t1,t2 with
      | FDD f1, FDD f2 -> lift_constants (FDD (TermMatrix.plus f1 f2))
      | Zero, _ -> t2
      | _, Zero -> t1
      | All, _ -> All
      | _, All -> All
      | Plus ts1, Plus ts2 -> Plus (MixedSet.union ts1 ts2)
      | Plus ts, _ -> Plus (MixedSet.add ts t2)
      | _, Plus ts -> Plus (MixedSet.add ts t1)
      | _, _ -> Plus (MixedSet.of_list [t1;t2])

    let times t1 t2 = match t1,t2 with
      | FDD f1, FDD f2 -> lift_constants (FDD (TermMatrix.times f1 f2))
      | One, _ -> t2
      | _, One -> t1
      | Zero, _ -> Zero
      | _, Zero -> Zero
      | Times ts, _ -> Times (ts @ [t2])
      | _, Times ts -> Times (t1 :: ts)
      | _, _ -> Times [t1;t2]

    (* let times ts = *)
    (*   let times' ts = match List.fold ts ~f:(fun (f_acc, t_acc) t -> *)
    (*       match f_acc, t with *)
    (*       | Some f, FDD f' -> Some (TermMatrix.times f f'), t_acc *)
    (*       | Some f, _ -> None, t :: FDD f :: t_acc *)
    (*       | None, FDD f -> Some f, t_acc *)
    (*       | None, One -> None, t_acc *)
    (*       | _, _ -> None, t :: t_acc) ~init:(None, []) with *)
    (*   | Some f, [] -> FDD f *)
    (*   | Some f, ts -> Times (List.rev (FDD f :: ts)) *)
    (*   | None, ts -> Times (List.rev ts) in *)
    (*   lift_constants (times' ts) *)

    (* TODO: Can probably do de Morgan to push comp up *)
    let intersection t1 t2 = match t1,t2 with
      | FDD f1, FDD f2 -> lift_constants (FDD (TermMatrix.intersection f1 f2))
      | Zero, _ -> Zero
      | _, Zero -> Zero
      | All, _ -> t2
      | _, All -> t1
      | Intersection ts1, Intersection ts2 -> Intersection (MixedSet.union ts1 ts2)
      | Intersection ts, _ -> Intersection (MixedSet.add ts t2)
      | _, Intersection ts -> Intersection (MixedSet.add ts t1)
      | _, _ -> Intersection (MixedSet.of_list [t1;t2])

    let intersections ts =
      let intersection' ts = let f, ts' = MixedSet.fold ts ~f:(fun (f_acc,set_acc) t -> match t,set_acc with
          | _, None -> TermMatrix.zero, None
          | FDD f, _ -> TermMatrix.intersection f f_acc, set_acc
          | All, _ -> f_acc, set_acc
          | Zero, _ -> TermMatrix.zero, None
          | _, Some set_acc -> f_acc, Some (MixedSet.add set_acc t))
          ~init:(TermMatrix.all, Some MixedSet.empty) in
        match ts' with
        | None -> zero
        | Some ts' ->
          if MixedSet.is_empty ts'
          then FDD f
          else if MixedSet.length ts' = 1
          then MixedSet.choose_exn ts'
          else Intersection (MixedSet.add ts' (FDD f)) in
      lift_constants (intersection' ts)

    let not t = match t with
      | FDD f -> lift_constants (FDD (TermMatrix.not f))
      | _ -> failwith "Negation of non-predicate!"

    let complement t = match t with
      | Complement t' -> t'
      | Zero -> All
      | All -> Zero
      | Intersection ts -> begin match MixedSet.for_all ts (fun t -> match t with
          | Complement _ -> true
          | _ -> false) with
        | true -> pluss (MixedSet.map ts (fun t -> match t with
            | Complement t -> t))
        | false -> Complement t
        end
      | Plus ts -> begin match MixedSet.for_all ts (fun t -> match t with
          | Complement _ -> true
          | _ -> false) with
        | true -> intersections (MixedSet.map ts (fun t -> match t with
            | Complement t -> t))
        | false -> Complement t
        end
      | _ -> Complement t

    let star t = match t with
      | FDD f -> FDD (TermMatrix.star f)
      | All -> All
      | One -> One
      | Zero -> One
      | _ -> Star t

    let rec matrix_of_term (t : Term.t) = let open Decide_Util.HashCons in
      match t.node with
      | Zero -> zero
      | One -> one
      | All -> all
      | Plus ts -> TermSet.fold ts ~init:Zero ~f:(fun acc t -> plus acc (matrix_of_term t))
      | Dup -> zero
      | Times ts -> List.fold ts ~init:One ~f:(fun acc t -> times acc (matrix_of_term t))
      | Star t -> star (matrix_of_term t)
      | Assg (f,v) -> assg f v
      | Test (f,v) -> test f v
      | Not t -> not (matrix_of_term t)
      | Complement t -> complement (matrix_of_term t)
      | Intersection ts -> TermSet.fold ts ~init:All ~f:(fun acc t -> intersection acc (matrix_of_term t))

    let union = plus

    let intersection_empty t1 t2 =
      let t = intersection t1 t2 in
      match t with
      | Zero -> true
      | One
      | All -> false
      | _ -> PointSet.is_empty (get_points t)

    let compare t1 t2 = match t1,t2 with
      | Zero, Zero
      | One, One
      | All, All -> 0
      | Zero, One
      | One, Zero
      | All, One
      |One, All
      | All, Zero
      | Zero, All -> -1
      | FDD f1, FDD f2 -> TermMatrix.compare f1 f2
      | _, _ -> PointSet.compare (get_points t1) (get_points t2)
  end

  module EMatrix = MixedTerm

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

    let compact_derivative_to_string elm = Printf.sprintf "(%s,%s)" (EMatrix.to_string elm.left_hand)
                                                                            (Term.to_string elm.right_hand)
    let to_string t = Printf.sprintf "{%s}" (String.concat ~sep:"; " (List.map (CompactDerivSet.elements t) compact_derivative_to_string))

    let pkt_to_beta pkt = Term.times (FieldMap.fold pkt ~init:[] ~f:(fun ~key ~data acc -> Term.test key data :: acc))
               
    let run t point =
      CompactDerivSet.fold t ~init:TermSet.empty
        ~f:(fun acc deriv ->
            let result = EMatrix.run deriv.left_hand point in
            (* Printf.printf "Running %s on %s: %b\n" (compact_derivative_to_string deriv) (point_to_string point) result; *)
               if result
             then TermSet.add acc (Term.times [pkt_to_beta (snd point); deriv.right_hand])
             else acc)

    let term_append t e = Term.times [t; e]
    let left_app e d = { d with left_hand = EMatrix.times e d.left_hand }
    let right_app d e = { d with right_hand = term_append d.right_hand e }

    let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e)
    let d_left_app e ds = CompactDerivSet.map ds (left_app e)


    let intersect d1 d2 =
      {left_hand = EMatrix.intersection d1.left_hand d2.left_hand;
       right_hand = Term.intersection (TermSet.of_list [d1.right_hand; d2.right_hand])}

    let matrix_of_term t =
      let rec matrix_of_term' t =
        let open Term in
        begin match t.node with
          | Dup             -> CompactDerivSet.singleton ({ left_hand = EMatrix.one;
                                                            right_hand = one })
          | Plus ts         -> TermSet.fold ts
                               ~f:(fun acc t -> CompactDerivSet.union (matrix_of_term' t) acc)
                               ~init:CompactDerivSet.empty
          | Times (t::ts)   -> CompactDerivSet.union (d_right_app (matrix_of_term' t) (times ts))
                                 (d_left_app (EMatrix.matrix_of_term t)
                                    (matrix_of_term' (times ts)))
          | Star t          -> d_left_app (EMatrix.matrix_of_term (star t))
                                 (d_right_app (matrix_of_term' t) (star t))
          | Intersection ts -> let intersect d1 d2 = CompactDerivSet.fold d1
              ~f:(fun acc x -> CompactDerivSet.union acc
                     (CompactDerivSet.map d2
                        ~f:(fun d ->
                          {left_hand = EMatrix.intersection d.left_hand x.left_hand;
                           right_hand = Term.intersection (TermSet.of_list [d.right_hand;
                                                                            x.right_hand])})))
              ~init:CompactDerivSet.empty in
            (* Can't have an empty intersection *)
            TermSet.fold ts ~f:(fun acc t -> intersect acc (matrix_of_term' t))
              ~init:(CompactDerivSet.singleton ({left_hand = EMatrix.matrix_of_term all; right_hand = Term.all}))
          | Complement t          -> let intersect d1 d2 = CompactDerivSet.fold d1
              ~f:(fun acc x ->
                  let e = EMatrix.intersection d2.left_hand x.left_hand in
                  let trans = Term.intersection (TermSet.of_list [(complement d2.right_hand);
                                                                  x.right_hand]) in
                  CompactDerivSet.add acc {left_hand = e;
                                           right_hand = trans})
              ~init:CompactDerivSet.empty
            in
            let d = CompactDerivSet.fold (matrix_of_term' t) ~f:intersect
                ~init:(CompactDerivSet.singleton ({left_hand = EMatrix.matrix_of_term all; right_hand = Term.all})) in
            let d_pos = CompactDerivSet.fold d ~f:(fun dom d ->
                EMatrix.union dom d.left_hand) ~init:EMatrix.empty in
            CompactDerivSet.add d {left_hand = EMatrix.complement d_pos;
                                   right_hand = Term.all}
          | All -> CompactDerivSet.singleton ({left_hand = EMatrix.matrix_of_term all; right_hand = Term.all})
          | Not _
          | Times []
          | Assg _
          | Test _
          | One
          | Zero            -> CompactDerivSet.empty
                    
        end in
      TermSet.fold t
        ~init:CompactDerivSet.empty
        ~f:(fun acc x -> CompactDerivSet.union acc (matrix_of_term' x))

    let rec power_set = function
      | [] -> [[]]
      | x :: xs -> let rem = power_set xs in
        rem @ (List.map rem (fun y -> x :: y))


    (*
       a) for each (b, e) \in D(elm1), (b',e') \in D(elm2),
          if b /\ b' != 0, then e bisim e'
       b) \/ b == \/ b'
    *)
    
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

  open HashCons
  open Term
  let rec run_e_exact t pkt = match t.node with
    | Assg (h,v) -> PacketSet.singleton (FieldMap.add pkt ~key:h ~data:v)
    | Test (h,v) -> begin match FieldMap.find pkt h with
        | Some v' -> if v' = v then PacketSet.singleton pkt else PacketSet.empty
        | None -> PacketSet.empty
      end
    | Dup -> PacketSet.empty
    | Plus ts -> TermSet.fold ts ~f:(fun acc t -> PacketSet.union acc (run_e_exact t pkt))
                   ~init:PacketSet.empty
    | Times (t::ts) -> PacketSet.fold (run_e_exact t pkt) ~f:(fun acc pkt' ->
        PacketSet.union acc (run_e_exact (times ts) pkt')) ~init:PacketSet.empty
    | Times [] -> PacketSet.singleton pkt
    | Star t -> let rec loop pkts gamma =
      let iter = run_e_exact t gamma in
      let pkts' = PacketSet.union pkts (PacketSet.singleton gamma) in
      if PacketSet.subset iter pkts' then
        pkts'
      else PacketSet.fold (PacketSet.diff iter pkts')
          ~f:loop ~init:pkts' in
      loop PacketSet.empty pkt
    | Intersection ts -> TermSet.fold ts ~f:(fun acc t -> PacketSet.inter acc (run_e_exact t pkt)) ~init:(run_e_exact (TermSet.choose_exn ts) pkt)
    | Complement t -> PacketSet.complement (run_e_exact t pkt)
    | Zero -> PacketSet.empty
    | One -> PacketSet.singleton pkt
    | All -> PacketSet.all
    | Not t -> match PacketSet.length (run_e_exact t pkt) with
      | 0 -> PacketSet.singleton pkt
      | 1 -> PacketSet.empty
      | _ -> failwith "called run_e_exact (Not t) where t was not a predicate!"

  let packet_to_term pkt = times (FieldMap.fold pkt ~init:[]
                                    ~f:(fun ~key ~data t -> (assg key data) :: t))

  let rec run_exact' t pt = match t.node with
    | Assg _
    | Test _ -> TermSet.empty
    | Dup -> if (fst pt) = (snd pt) then TermSet.singleton (packet_to_term (fst pt)) else TermSet.empty
    | Plus ts -> TermSet.fold ts ~f:(fun acc t -> TermSet.union acc (run_exact' t pt)) ~init:TermSet.empty
    (* TODO: Smart way to do this is to "extract" the gammas that make the product non-zero instead of iterating over all possible ones *)
    | Times (t::ts) -> TermSet.union (TermSet.singleton (times (plus (run_exact' t pt) :: ts)))
                         (PacketSet.fold (run_e_exact t (fst pt))
                            ~f:(fun acc pkt -> TermSet.union acc (run_exact' (times ts) (pkt, snd pt))) ~init:TermSet.empty)
    | Times [] -> TermSet.empty
    | Intersection ts -> TermSet.singleton (intersection (TermSet.fold ts ~f:(fun acc t -> TermSet.union acc (run_exact' t pt)) ~init:TermSet.empty))
    | Not t -> TermSet.empty
    | Complement t -> TermSet.singleton (complement (plus (run_exact' t pt)))
    | Star t -> let rec loop ts gamma =
      let iter = TermSet.map (run_exact' t (gamma, snd pt)) ~f:(fun e -> times [e; star t]) in
      if TermSet.subset iter ts
      then
        ts
      else
        PacketSet.fold (run_e_exact t gamma) ~f:loop ~init:(TermSet.union ts iter) in
      (loop TermSet.empty (fst pt))
    | Zero -> TermSet.empty
    | One -> TermSet.empty
    | All -> TermSet.singleton all

  let run_exact t pt = match t.d_matrix with
    | Some d -> DMatrix.run d pt
    | None -> run_exact' (plus t.desc) pt

  let compare t t' = TermSet.compare t.desc t'.desc
  let to_string t = Printf.sprintf "[desc: %s;\n e_matrix: %s;\n d_matrix: %s]"
      (TermSet.to_string t.desc)
      (EMatrix.to_string (get_e t))
      (DMatrix.to_string (get_d t))
end
