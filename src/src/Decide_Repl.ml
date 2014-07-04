open Decide_Util

type state = int
let init_state = 0
    
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

let split_string (sr : string) (c : char) : string list = 
  let l,acc = string_fold (fun e (l,acc) -> 
    if e = c then 
      "",l::acc 
    else (l ^ (Char.escaped e)), acc) sr ("",[]) in 
  List.rev (l::acc)
 
      

let proc_loop (input : string) : unit =
  let open Decide_Ast.Formula in 
  try
    let (edge,_),(pol,_),(topo,_) = match split_string input '%' with 
      | [edge;pol;topo] -> 
	terms (parse (edge ^ " == drop")),
	terms (parse (pol ^ " == drop")),
	terms (parse (topo ^ " == drop"))
      | _ -> failwith "parse error!" in 
    Printf.printf "unfolded\n%!";
    Printf.printf "edge policy %s\npol: %s\ntopo: %s\n "
      (Decide_Ast.Term.to_string edge)
      (Decide_Ast.Term.to_string pol)
      (Decide_Ast.Term.to_string topo);
    Printf.printf "Loop-freedom result: %b\n"
      (Decide_Loopfree.loop_freedom edge pol topo ())
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
  let run_loop = input = "loop" in
  let input = if run_loop then (print_string "> "; read_line ()) else input in 
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
      if run_loop 
      then proc_loop input 
      else process input;
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
