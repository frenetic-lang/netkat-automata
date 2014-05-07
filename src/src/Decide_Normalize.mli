type ssm_pair = Decide_Util.IntSetMap.t * Decide_Util.IntSetMap.t

module B : sig
  include Set.S with type elt = ssm_pair
end

val universe : Decide_Ast.formula -> Decide_Util.IntSetMap.t
val fillout_ssm_pairset : Decide_Util.IntSetMap.t -> B.t -> B.t
val reduced : Decide_Util.IntSetMap.t -> Decide_Util.IntSetMap.t -> unit
val leq_ssm_pairset : ssm_pair -> B.t -> bool
val forall_atoms : (ssm_pair -> bool) -> ssm_pair -> bool
