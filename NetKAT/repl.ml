open Util

type state = int
let init_state = 0

let process (input : string) : unit =
  let parsed = Parse.parseFormula input in
  let ps = Ast.simplify_formula parsed in
  print_string (Ast.formula_to_string ps);
  print_newline ()
  
(* read from a file *)
let load (filename : string) : string list =
  let rec get_contents contents file =
    let input_line_option file =
      try Some (input_line file) with End_of_file -> None in
    match (input_line_option file) with
      Some x -> get_contents (x :: contents) file
    | None -> contents in
  try
    let file = open_in filename in
    let result = get_contents [] file in close_in file; List.rev result
  with Sys_error msg ->
    print_endline msg; []
			
let process_file (filename : string) : unit =
	List.iter process (load filename)

(* command loop *)
let rec repl (state : state) : unit =
  print_string "? ";
  let input = read_line() in
  if input = "quit" then raise Quit;
  (try
    process input
  with Parsing.Parse_error ->
    print_endline "Parse error");
  repl state

let _ =
  (if (Array.length Sys.argv) > 1 then
    let args = List.tl (Array.to_list Sys.argv) in
    List.iter process_file args
  else
    print_endline "NetKAT");
    try repl init_state
    with Quit -> print_endline "bye"
