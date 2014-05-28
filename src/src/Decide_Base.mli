  module Base : sig
    type t
    val compare : t -> t -> int
    val univ_base : unit -> t
    val of_assg : Decide_Util.Field.t -> Decide_Util.Value.t -> t
    val of_test : Decide_Util.Field.t -> Decide_Util.Value.t -> t
    val of_neg_test : Decide_Util.Field.t -> Decide_Util.Value.t -> t

    type point
    val compare_point : point -> point -> int
    val point_to_string : point -> string
    type complete_test 
    val point_rhs : point -> complete_test
    val point_lhs : point -> complete_test
    val compare_complete_test : complete_test -> complete_test -> int
    val complete_test_to_string : complete_test -> string

    val project_lhs : t -> t
    module Set : sig 
      include Set.S with type elt = t
      val shallow_equal : t -> t -> bool
      val to_string : t -> string
      val fold_points : (point -> 'a -> 'a) -> t -> 'a -> 'a
      val contains_point : t -> point -> bool
      val filter_alpha : t -> complete_test -> t
      val mult : t -> t -> t

    (* debugging *)
      val print_debugging_info : unit -> unit
    end
  end
