open Decide_Util

type state = int
let init_state = 0

(* returns true if universe is non-empty *)
let set_univ (tvallist : Decide_Ast.UnivMap.t list) : bool = 
  let module UnivMap = Decide_Util.SetMapF (Decide_Util.Field) (Decide_Util.Value) in
  let univ = List.fold_right UnivMap.union tvallist UnivMap.empty in 
  let univ = List.fold_left (fun u x -> UnivMap.add x Value.extra_val u) univ (UnivMap.keys univ) in
  let module UnivDescr = struct
	let all_fields : Decide_Util.FieldSet.t = 
	  (* TODO: fix me when SSM is eliminated *)
	  List.fold_right 
	    (fun f -> 
	      Printf.printf "adding field to universe: %s\n" (Decide_Util.Field.to_string f);
	      FieldSet.add f) (UnivMap.keys univ) FieldSet.empty
	let all_values f : Decide_Util.ValueSet.t = 
	  try 
	    UnivMap.Values.fold (fun v acc -> Decide_Util.ValueSet.add v acc ) (UnivMap.find_all f univ) 
	      Decide_Util.ValueSet.empty
	  with Not_found -> 
	    Decide_Util.ValueSet.empty
      end in   
  Decide_Util.all_fields := (fun _ -> UnivDescr.all_fields);
  Decide_Util.all_values := (fun _ -> UnivDescr.all_values);
  List.exists (fun e -> not (UnivMap.is_empty e)) tvallist

let loop_freedom trm = 
  let open Decide_Ast in 
  let open Decide_Base in 
  let open Decide_Deriv in 
  let trm_vals = Term.values trm in 
  if set_univ [trm_vals]
  then 
	begin
	  let dtrm = DerivTerm.make_term trm in 
	  let dmat,pset = DerivTerm.run_d dtrm in 
	  Base.Set.fold_points 
		(fun pt acc -> 
		  let beta_t = Term.of_complete_test (Base.point_rhs pt) in
		  let alpha_t = Term.of_complete_test (Base.point_lhs pt) in
		  let dtrm_t = DerivTerm.to_term (dmat pt) in 
		  let newterm = 
			(Term.make_times 
			   [beta_t; Term.make_star dtrm_t ; alpha_t]) in
		  let em = Term.one_dup_e_matrix newterm in 
		  (Base.Set.is_empty em) && acc
		) pset true
	end
  else true

let run_bisimulation t1 t2 = 
  let t1vals = Decide_Ast.Term.values t1 in 
  let t2vals = Decide_Ast.Term.values t2 in 
  if set_univ [t1vals; t2vals]
  then Decide_Bisimulation.check_equivalent t1 t2
  else Decide_Ast.Term.equal t1 t2

exception ParseError of int * int * string
                              
let parse (s : string) = 
  let lexbuf = Lexing.from_string s in
  (try
     Decide_Parser.formula_main Decide_Lexer.token lexbuf
   with
     | Parsing.Parse_error ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let char = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let token = Lexing.lexeme lexbuf in
    raise (ParseError (line, char, token)))

let process (input : string) : unit =
  try
    let parsed = parse input in
    let l,r = Decide_Ast.Formula.terms parsed in 
    Printf.printf "unfolded\n%!";
    Printf.printf "LHS term:%s\n" (Decide_Ast.Term.to_string l);
    Printf.printf "RHS term:%s\n" (Decide_Ast.Term.to_string r);
    Printf.printf "Bisimulation result: %b\n"
      (run_bisimulation l r )
  with
  | Decide_Ast.Empty -> 
    ()
  | Decide_Lexer.LexError s -> 
    Printf.printf "Lex Error: %s\n" s
  | ParseError (l, ch, t) ->
    Printf.printf "Syntax error at line %d, char %d, token \'%s\'\n" l ch t
  
(* read from a file *)
let load (filename : string) : string option =
  let rec get_contents contents file =
    let input_line_option file =
      try Some (input_line file) with End_of_file -> None in
    match (input_line_option file) with
    | Some x -> get_contents (x :: contents) file
    | None -> contents in
  try
    let file = open_in filename in
    let result = get_contents [] file in
    close_in file;
    Some (String.concat " " (List.rev result))
  with Sys_error msg ->
    print_endline msg; None
			
let process_file (filename : string) : unit =
  match (load filename) with
  | Some s -> process s
  | None -> ()

(* command loop *)
let rec repl (state : state) : unit =
  print_string "? ";
  let input = read_line() in
  if input = "quit" then raise Quit;
  let input = if input = "load" 
    then (print_string ": ";
	  match (load (read_line ())) with 
	    | Some s -> s
	    | None -> failwith "file load didn't work"
    ) 
    else input in
  print_string "process or serialize: ";
  (match (* read_line() *) "process"  with 
    | "process" ->
      Printf.printf "processing...\n%!";
      process input;
    | "serialize" -> 
      print_string "where: ";
      let file = read_line () in 
      let formula = parse input in 
      ignore file; 
      ignore formula;
      failwith "mode not currently supported"
    | _ -> repl state);
  repl state

let main () =
  (if (Array.length Sys.argv) > 1 then
    let args = List.tl (Array.to_list Sys.argv) in
    List.iter process_file args
  else
    print_endline "NetKAT");
    try repl init_state
    with Quit -> print_endline "bye"

let _ = main ()
