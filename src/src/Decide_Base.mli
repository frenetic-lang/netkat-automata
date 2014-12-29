module PosNeg : sig
  type t
  val to_string : t -> string
  val empty : Decide_Util.Field.t -> t
  val singleton : Decide_Util.Field.t -> Decide_Util.ValueSet.elt -> t
  val any : Decide_Util.Field.t -> t
  val add : Decide_Util.ValueSet.elt -> t -> t
  val union : t -> t -> t
  (* pre: pn1 and pn2 should be defined over the same field *)
  val compare : t -> t -> int
  val contains : t -> Decide_Util.Value.t -> bool
  val intersect : t -> t -> t
  val is_empty : t -> bool
  val subset : t -> t -> bool
  val elements : t -> Decide_Util.ValueSet.t
	  
end (* PosNeg *)


module Base : sig

  module Map : sig
    type key = Decide_Util.Field.t
    type 'a t with sexp
    val find : key -> 'a t -> 'a
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val empty : unit -> 'a t
  end
  
  type t with sexp
  type assg = Decide_Util.ValueSet.elt Map.t with sexp
    val compare : t -> t -> int
    val univ_base : unit -> t
    val of_assg : Decide_Util.Field.t -> Decide_Util.Value.t -> t
    val of_test : Decide_Util.Field.t -> Decide_Util.Value.t -> t
    val of_neg_test : Decide_Util.Field.t -> Decide_Util.Value.t -> t

    type point = Point of assg * assg with sexp
    val compare_point : point -> point -> int
    val point_to_string : point -> string
    type complete_test with sexp
    val point_rhs : point -> complete_test
    val point_lhs : point -> complete_test
    val compare_complete_test : complete_test -> complete_test -> int
    val complete_test_to_string : complete_test -> string
    val complete_test_vals : complete_test -> (Decide_Util.Field.t * Decide_Util.Value.t) list
    val complete_test_to_point : complete_test -> complete_test -> point

    val project_lhs : t -> t
    module Set : sig 
      include Core.Std.Set.S with type Elt.t = t
      val add : Elt.t -> t -> t
      val t_of_sexp : Sexplib.Sexp.t -> t
      val sexp_of_t : t -> Sexplib.Sexp.t
      val shallow_equal : t -> t -> bool
      val compact : t -> t
      val to_string : t -> string
      val fold_points : (point -> 'a -> 'a) -> t -> 'a -> 'a
      val contains_point : t -> point -> bool
      val filter_alpha : t -> complete_test -> t
      val mult : t -> t -> t

    (* debugging *)
      val print_debugging_info : unit -> unit
    end
  end
