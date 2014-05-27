open Decide_Util

type state = int
let init_state = 0

let convert_term t = failwith "later" (* match t with 
  | Dup -> 
  | One (-1,None) -> 
  | Zero (-1,None) ->
  | Assg(-1,f,v,None) -> 
  | Test(-1,f,v,None) -> 
  | Plus (-1,ts,None) -> 
  | Times (-1,tl,None) -> 
  | Not (-1, t,None) -> 
  | Star (-1, t,None) -> 
				      *)

let run_bisimulation t1 t2 = 

  let module UnivMap = Decide_Util.SetMapF (Decide_Util.Field) (Decide_Util.Value) in
  let module InitialAst = Decide_Ast.Ast(Decide_Ast.DummyUniv) in 
  let t1vals = InitialAst.values_in_term t1 in 
  let t2vals = InitialAst.values_in_term t2 in 
  if ((not (UnivMap.is_empty t1vals)) || (not (UnivMap.is_empty t2vals)))
  then 
    begin
      let univ = UnivMap.union t1vals t2vals in 
      let univ = List.fold_left (fun u x -> UnivMap.add x Value.extra_val u) univ (UnivMap.keys univ) in
      let module UnivDescr = struct
	let all_fields : Decide_Util.FieldSet.t = 
	    (* TODO: fix me when SSM is eliminated *)
	  List.fold_right FieldSet.add (UnivMap.keys univ) FieldSet.empty
	let _ = assert (FieldSet.cardinal all_fields > 0 )
	let all_values f : Decide_Util.ValueSet.t = 
	  try 
	    UnivMap.Values.fold (fun _ _ -> failwith "hi" ) (UnivMap.find_all f univ) 
	      Decide_Util.ValueSet.empty
	  with Not_found -> 
	    Decide_Util.ValueSet.empty
      end in   
      
      let module Bisimulation = Decide_Bisimulation.Bisimulation(UnivDescr) in 
      Bisimulation.check_equivalent (convert_term t1) (convert_term t2)
    end      
  else (
    Printf.eprintf "comparing empty terms!\n";
    true)



exception ParseError of int * int * string
                              
let parse (s : string) = 
  let lexbuf = Lexing.from_string s in
  (try
     Parser.formula_main Lexer.token lexbuf
   with
     | Parsing.Parse_error ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let char = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let token = Lexing.lexeme lexbuf in
    raise (ParseError (line, char, token)))

let process (input : string) : unit =
  let module Decide_Ast' = Decide_Ast.Ast(Decide_Ast.DummyUniv) in 
  try
    let parsed = Decide_Ast'.convert_and_simplify parse input in
    let l,r = Decide_Ast'.terms_in_formula parsed in 
    Printf.printf "Left term in formula: %s\n" (Decide_Ast'.Term.to_string l);
    Printf.printf "Right term in formula: %s\n" (Decide_Ast'.Term.to_string r);
    Printf.printf "Bisimulation result: %b\n"
      (run_bisimulation l r )

  with
  | Decide_Ast.Empty -> ()
  | Lexer.LexError s -> Printf.printf "Lex Error: %s\n" s
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
      let module Decide_Ast = Decide_Ast.Ast(Decide_Ast.DummyUniv) in
      let formula = Decide_Ast.convert_and_simplify parse input in 
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
