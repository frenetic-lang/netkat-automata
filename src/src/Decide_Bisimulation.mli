
module Bisimulation : functor (UDesc : Decide_Base.UnivDescr) -> sig 

  val check_equivalent : Decide_Ast.Ast(UDesc).term -> Decide_Ast.Ast(UDesc).term -> bool 
    
end

(* 

stashing this here for a sec

  (* TODO: this is a heuristic.  Which I can spell, hooray.  *)
    let module UnivMap = Decide_Util.SetMapF (Decide_Util.Field) (Decide_Util.Value) in
    let t1vals = Ast.values_in_term t1 in 
    let t2vals = Ast.values_in_term t2 in 
    if ((not (UnivMap.is_empty t1vals)) || (not (UnivMap.is_empty t2vals)))
    then 
      begin
	let univ = UnivMap.union t1vals t2vals in 
	let univ = List.fold_left (fun u x -> UnivMap.add x Value.extra_val u) univ (UnivMap.keys univ) in
	let module UnivDescr = struct
	  type field = Decide_Util.Field.t
	  type value = Decide_Util.Value.t
	  module FieldSet = Set.Make(Decide_Util.Field)
	  module ValueSet = UnivMap.Values
	  let all_fields = 
	  (* TODO: fix me when SSM is eliminated *)
	    List.fold_right FieldSet.add (UnivMap.keys univ) FieldSet.empty
	  let _ = assert (FieldSet.cardinal all_fields > 0 )
	  let all_values f = 
	    try 
	      UnivMap.find_all f univ
	    with Not_found -> 
	      ValueSet.empty
	end in   
	
	let module Decide_Ast = Decide_Ast.Ast(UnivDescr) in 
	 (* actually run bisimulation *)
	


    else (
      Printf.eprintf "comparing empty terms!\n";
      true)

*)
