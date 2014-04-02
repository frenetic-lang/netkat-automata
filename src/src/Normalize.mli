type ssm_pair = Util.StringSetMap.t * Util.StringSetMap.t

module B : sig
  include Set.S with type elt = ssm_pair
end

val universe : Ast.formula -> Util.StringSetMap.t
val fillout_ssm_pairset : Util.StringSetMap.t -> B.t -> B.t
val reduced : Util.StringSetMap.t -> Util.StringSetMap.t -> unit
val leq_ssm_pairset : ssm_pair -> B.t -> bool
val forall_atoms : (ssm_pair -> bool) -> ssm_pair -> bool
